<template>
  <div class="smart-table">
    <div class="st-toolbar">
      <div class="st-left">
        <slot name="left-toolbar">
          <div v-if="filterable" class="st-filter">
            <input
              v-model="localFilter"
              @input="onFilterInput"
              :placeholder="filterPlaceholder"
              class="st-input"
            />
          </div>
        </slot>
      </div>

      <div class="st-right">
        <slot name="right-toolbar">
          <button v-if="showExport" @click="exportCSV" class="st-btn">Export CSV</button>
        </slot>
      </div>
    </div>

    <div class="st-table-wrap">
      <table class="st-table">
        <thead>
          <tr>
            <th v-if="selectable">
              <input type="checkbox" :checked="allSelected" @change="toggleSelectAll" />
            </th>

            <th
              v-for="(col, idx) in visibleColumns"
              :key="col.key || idx"
              :style="col.style"
              class="st-th"
            >
              <div class="st-th-inner" @click="toggleSort(col)" :title="col.label">
                <slot :name="`header-${col.key}`">{{ col.label }}</slot>
                <span v-if="col.sortable && currentSort.key === col.key" class="st-sort">
                  {{ currentSort.order === 'asc' ? '▲' : '▼' }}
                </span>
              </div>
            </th>

            <th v-if="$slots.actions || actionsColumn">
              <slot name="header-actions">Actions</slot>
            </th>
          </tr>
        </thead>

        <tbody>
          <tr v-for="(row, rIdx) in pageRows" :key="rowKey(row, rIdx)">
            <td v-if="selectable">
              <input type="checkbox" :value="row" :checked="isSelected(row)" @change="toggleSelect(row)" />
            </td>

            <td
              v-for="(col, cIdx) in visibleColumns"
              :key="col.key || cIdx"
              class="st-td"
            >
              <slot :name="`cell-${col.key}`" :row="row" :value="getValue(row, col)">
                <!-- default cell rendering -->
                <div v-if="!col.template">
                  <span v-if="!col.editable || !isEditing(row, col)">{{ formatValue(getValue(row, col), col) }}</span>
                  <input
                    v-else-if="col.editable && isEditing(row, col)"
                    :type="col.inputType || 'text'"
                    v-model="editBuffer[col.key]"
                    @blur="commitEdit(row, col)"
                    @keyup.enter="commitEdit(row, col)"
                  />
                </div>
                <div v-else v-html="col.template(getValue(row, col), row)"></div>
              </slot>
            </td>

            <td v-if="$slots.actions || actionsColumn">
              <slot name="actions" :row="row">
                <button v-if="editable" class="st-btn" @click="startEdit(row)">Edit</button>
                <button class="st-btn st-danger" @click="$emit('delete-row', row)">Delete</button>
              </slot>
            </td>
          </tr>

          <tr v-if="pageRows.length === 0">
            <td :colspan="baseColspan" class="st-empty">No data</td>
          </tr>
        </tbody>
      </table>
    </div>

    <div class="st-footer">
      <div class="st-pagination">
        <button class="st-btn" :disabled="page === 1" @click="changePage(1)">« First</button>
        <button class="st-btn" :disabled="page === 1" @click="changePage(page - 1)">‹ Prev</button>

        <span class="st-page-info">Page</span>
        <input class="st-page-input" type="number" v-model.number="pageInput" @keyup.enter="goToPage" />
        <span>/ {{ totalPages }}</span>

        <button class="st-btn" :disabled="page === totalPages" @click="changePage(page + 1)">Next ›</button>
        <button class="st-btn" :disabled="page === totalPages" @click="changePage(totalPages)">Last »</button>

        <select v-model.number="pageSize" @change="onPageSizeChange" class="st-select">
          <option v-for="opt in pageSizeOptions" :key="opt" :value="opt">{{ opt }}</option>
        </select>
      </div>

      <div class="st-summary">
        <slot name="summary">
          Showing {{ pageRows.length }} of {{ total }} rows
        </slot>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed, watch, reactive, toRefs } from 'vue'
import debounce from './utils/debounce'

// Props
const props = defineProps({
  columns: { type: Array, default: () => [] },
  data: { type: Array, default: () => [] },
  total: { type: Number, default: null },
  serverMode: { type: Boolean, default: false },
  pageSizeOptions: { type: Array, default: () => [10, 25, 50, 100] },
  defaultPageSize: { type: Number, default: 10 },
  defaultPage: { type: Number, default: 1 },
  rowKeyField: { type: String, default: null },
  sortable: { type: Boolean, default: true },
  filterable: { type: Boolean, default: true },
  filterPlaceholder: { type: String, default: 'Search...' },
  selectable: { type: Boolean, default: false },
  editable: { type: Boolean, default: false },
  actionsColumn: { type: Boolean, default: false },
  showExport: { type: Boolean, default: true },
})

const emit = defineEmits(['update:page', 'update:pageSize', 'sort', 'filter', 'selection-change', 'edit', 'row-click'])

const page = ref(props.defaultPage)
const pageSize = ref(props.defaultPageSize)
const pageInput = ref(props.defaultPage)
const localFilter = ref('')
const currentSort = reactive({ key: null, order: null })

const selected = ref(new Set())

const editing = ref(null)
const editBuffer = reactive({})

const visibleColumns = computed(() => props.columns.filter(c => c.visible !== false))

const baseColspan = computed(() => visibleColumns.value.length + (props.selectable ? 1 : 0) + (props.actionsColumn || !!getCurrentSlots().actions ? 1 : 0))

const clientTotal = computed(() => (props.data || []).length)
const totalRows = computed(() => (props.total === null ? clientTotal.value : props.total))
const totalPages = computed(() => Math.max(1, Math.ceil(totalRows.value / pageSize.value)))

const getValue = (row, col) => {
  if (!col) return ''
  if (typeof col.accessor === 'function') return col.accessor(row)
  return row[col.key]
}

const formatValue = (val, col) => {
  if (col && col.format) return col.format(val)
  if (val === null || val === undefined) return ''
  return String(val)
}

const pageRows = computed(() => {
  if (props.serverMode) return props.data || []

  let rows = (props.data || []).slice()

  if (localFilter.value) {
    const q = localFilter.value.toLowerCase()
    rows = rows.filter(r => visibleColumns.value.some(c => ('' + (getValue(r, c) ?? '')).toLowerCase().includes(q)))
  }

  if (currentSort.key) {
    const col = visibleColumns.value.find(c => c.key === currentSort.key)
    if (col) {
      rows.sort((a, b) => {
        const av = getValue(a, col)
        const bv = getValue(b, col)
        if (av == null && bv == null) return 0
        if (av == null) return -1
        if (bv == null) return 1
        if (av === bv) return 0
        return (av > bv ? 1 : -1) * (currentSort.order === 'asc' ? 1 : -1)
      })
    }
  }

  const start = (page.value - 1) * pageSize.value
  return rows.slice(start, start + pageSize.value)
})

const changePage = (p) => {
  page.value = Math.max(1, Math.min(totalPages.value, p))
  pageInput.value = page.value
  emit('update:page', page.value)
}

const goToPage = () => changePage(pageInput.value)

const onPageSizeChange = () => {
  page.value = 1
  pageInput.value = 1
  emit('update:pageSize', pageSize.value)
}

const toggleSort = (col) => {
  if (!props.sortable || !col.sortable) return
  if (currentSort.key !== col.key) {
    currentSort.key = col.key
    currentSort.order = 'asc'
  } else if (currentSort.order === 'asc') {
    currentSort.order = 'desc'
  } else {
    currentSort.key = null
    currentSort.order = null
  }
  emit('sort', { key: currentSort.key, order: currentSort.order })
}

const onFilterInput = debounce(() => {
  page.value = 1
  pageInput.value = 1
  emit('filter', localFilter.value)
}, 300)

const rowKey = (row, idx) => (props.rowKeyField ? row[props.rowKeyField] : idx)

const isSelected = (row) => selected.value.has(rowKey(row))

const toggleSelect = (row) => {
  const k = rowKey(row)
  if (selected.value.has(k)) selected.value.delete(k)
  else selected.value.add(k)
  emit('selection-change', Array.from(selected.value))
}

const allSelected = computed(() => {
  if (!props.data || props.data.length === 0) return false
  const pageKeys = pageRows.value.map((r, i) => rowKey(r, i))
  return pageKeys.every(k => selected.value.has(k))
})

const toggleSelectAll = () => {
  const pageKeys = pageRows.value.map((r, i) => rowKey(r, i))
  const all = pageKeys.every(k => selected.value.has(k))
  if (all) pageKeys.forEach(k => selected.value.delete(k))
  else pageKeys.forEach(k => selected.value.add(k))
  emit('selection-change', Array.from(selected.value))
}

const startEdit = (row) => {
  if (!props.editable) return
  editing.value = { rowKey: rowKey(row), colKey: null }
  visibleColumns.value.forEach(c => {
    if (c.editable) editBuffer[c.key] = getValue(row, c)
  })
}

const isEditing = (row, col) => editing.value && editing.value.rowKey === rowKey(row) && (!col || editing.value.colKey === col.key)

const commitEdit = (row, col) => {
  if (!props.editable) return
  const k = rowKey(row)
  const payload = { rowKey: k, updates: {} }
  if (col) {
    payload.updates[col.key] = editBuffer[col.key]
  } else {
    visibleColumns.value.forEach(c => { if (c.editable) payload.updates[c.key] = editBuffer[c.key] })
  }
  emit('edit', payload)
  editing.value = null
}

const exportCSV = () => {
  const hdrs = visibleColumns.value.map(c => c.label || c.key)
  const rows = (props.data || []).map(r => visibleColumns.value.map(c => JSON.stringify(getValue(r, c) ?? '')))
  const csv = [hdrs.join(','), ...rows.map(r => r.join(','))].join('\n')
  const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = 'export.csv'
  a.click()
  URL.revokeObjectURL(url)
}

watch(() => props.data, () => {
  if (!props.serverMode && page.value > totalPages.value) {
    changePage(totalPages.value)
  }
})

function getCurrentSlots() {
  return Object.keys(useSlots ? useSlots() : {}).reduce((acc, k) => { acc[k] = true; return acc }, {})
}
</script>

<style scoped>
.smart-table { font-family: system-ui, Arial; border: 1px solid #e6e6e6; border-radius: 6px; overflow: hidden }
.st-toolbar { display:flex; justify-content:space-between; gap:12px; padding:12px; background:#fafafa; align-items:center }
.st-input { padding:6px 8px; border:1px solid #ddd; border-radius:4px }
.st-btn { padding:6px 8px; border-radius:6px; border:1px solid #ddd; background:white; cursor:pointer }
.st-btn.st-danger { border-color:#f5c6cb }
.st-table-wrap { overflow:auto }
.st-table { width:100%; border-collapse:collapse }
.st-th, .st-td { padding:10px; border-bottom:1px solid #f1f1f1; text-align:left }
.st-th-inner { display:flex; align-items:center; gap:8px; cursor:pointer }
.st-sort { font-size:10px }
.st-empty { text-align:center; padding:24px; color:#777 }
.st-footer { display:flex; justify-content:space-between; align-items:center; padding:12px; background:#fafafa }
.st-pagination { display:flex; gap:8px; align-items:center }
.st-page-input { width:56px; padding:6px }
.st-select { padding:6px }
</style>
