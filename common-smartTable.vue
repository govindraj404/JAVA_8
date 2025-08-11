<script setup>
/*
  CommonSmartTable
  ----------------
  This component wraps CoreUI's <CTable> and adds smart-table-like features:
    ✅ Column-based sorting (click on header to toggle ascending/descending)
    ✅ Search across all columns
    ✅ Optional row selection with checkbox support
    ✅ Optional pagination (numbered: << < 1 2 3 4 5 > >>) — max 5 visible pages
    ✅ Pagination aligned to bottom-right with customizable color
    ✅ Works with any data passed as array of objects
    ✅ Controlled page size via props
    ✅ Emits 'selection-change' event when selected rows change
*/

import { ref, computed } from 'vue'
import {
  CTable, CTableHead, CTableBody, CTableRow, CTableHeaderCell, CTableDataCell,
  CPagination, CPaginationItem, CFormInput, CCard, CCardHeader, CCardBody, CCardFooter
} from '@coreui/vue'

// ----------- Props (passed from parent) -----------
const props = defineProps({
  columns: { type: Array, required: true },     // [{ key: 'id', label: 'ID' }, ...]
  data: { type: Array, required: true },        // Table data array
  rowKeyField: { type: String, default: 'id' }, // Field used as unique key for rows
  selectable: { type: Boolean, default: false },// Whether rows can be selected
  paginated: { type: Boolean, default: true },  // Enable/disable pagination
  pageSize: { type: Number, default: 10 }       // Rows per page
})

// ----------- Events emitted to parent -----------
const emit = defineEmits(['selection-change'])

// ----------- Reactive State -----------
const searchQuery = ref('')     // Search input text
const page = ref(1)             // Current page number
const sortKey = ref(null)       // Currently sorted column key
const sortOrder = ref('asc')    // 'asc' or 'desc'
const selectedRows = ref([])    // Array of selected rows

// ----------- Computed: Search filter -----------
const filteredData = computed(() => {
  if (!searchQuery.value) return props.data
  return props.data.filter(row =>
    Object.values(row).some(v =>
      String(v).toLowerCase().includes(searchQuery.value.toLowerCase())
    )
  )
})

// ----------- Computed: Sorting -----------
const sortedData = computed(() => {
  if (!sortKey.value) return filteredData.value
  return [...filteredData.value].sort((a, b) => {
    const valA = a[sortKey.value]
    const valB = b[sortKey.value]
    if (valA < valB) return sortOrder.value === 'asc' ? -1 : 1
    if (valA > valB) return sortOrder.value === 'asc' ? 1 : -1
    return 0
  })
})

// ----------- Computed: Total pages -----------
const totalPages = computed(() => Math.ceil(sortedData.value.length / props.pageSize))

// ----------- Computed: Paginated rows -----------
const displayedRows = computed(() => {
  if (!props.paginated) return sortedData.value
  const start = (page.value - 1) * props.pageSize
  return sortedData.value.slice(start, start + props.pageSize)
})

// ----------- Computed: Visible pagination pages (max 5) -----------
const visiblePages = computed(() => {
  const total = totalPages.value
  const current = page.value
  const maxVisible = 5
  let start = Math.max(1, current - Math.floor(maxVisible / 2))
  let end = start + maxVisible - 1
  if (end > total) {
    end = total
    start = Math.max(1, end - maxVisible + 1)
  }
  const pages = []
  for (let i = start; i <= end; i++) pages.push(i)
  return pages
})

// ----------- Methods: Toggle sorting -----------
function toggleSort(key) {
  if (sortKey.value === key) {
    sortOrder.value = sortOrder.value === 'asc' ? 'desc' : 'asc'
  } else {
    sortKey.value = key
    sortOrder.value = 'asc'
  }
}

// ----------- Methods: Toggle row selection -----------
function toggleSelect(row) {
  const index = selectedRows.value.findIndex(r => r[props.rowKeyField] === row[props.rowKeyField])
  if (index >= 0) {
    selectedRows.value.splice(index, 1)
  } else {
    selectedRows.value.push(row)
  }
  emit('selection-change', selectedRows.value)
}
</script>

<template>
  <CCard>
    <!-- Search Box -->
    <CCardHeader>
      <CFormInput v-model="searchQuery" placeholder="Search..." size="sm" />
    </CCardHeader>

    <!-- Table -->
    <CCardBody>
      <CTable hover striped responsive>
        <!-- Table Header with sorting -->
        <CTableHead>
          <CTableRow>
            <!-- Checkbox header if selectable -->
            <CTableHeaderCell v-if="selectable"></CTableHeaderCell>
            
            <!-- Column headers -->
            <CTableHeaderCell
              v-for="col in columns"
              :key="col.key"
              @click="toggleSort(col.key)"
              style="cursor: pointer"
            >
              {{ col.label }}
              <!-- Sort indicator -->
              <span v-if="sortKey === col.key">
                {{ sortOrder === 'asc' ? '▲' : '▼' }}
              </span>
            </CTableHeaderCell>
          </CTableRow>
        </CTableHead>

        <!-- Table Body -->
        <CTableBody>
          <CTableRow v-for="row in displayedRows" :key="row[rowKeyField]">
            <!-- Row selection checkbox -->
            <CTableDataCell v-if="selectable">
              <input
                type="checkbox"
                :checked="selectedRows.includes(row)"
                @change="toggleSelect(row)"
              />
            </CTableDataCell>

            <!-- Row data cells -->
            <CTableDataCell v-for="col in columns" :key="col.key">
              {{ row[col.key] }}
            </CTableDataCell>
          </CTableRow>
        </CTableBody>
      </CTable>
    </CCardBody>

    <!-- Pagination -->
    <CCardFooter v-if="paginated" class="d-flex justify-content-end align-items-center">
      <CPagination
        size="sm"
        class="mb-0"
        style="--cui-pagination-color: red;"  <!-- Red color -->
      >
        <!-- First page -->
        <CPaginationItem :disabled="page === 1" @click="page = 1">&laquo;</CPaginationItem>

        <!-- Previous page -->
        <CPaginationItem :disabled="page === 1" @click="page--">&lt;</CPaginationItem>

        <!-- Numbered pages -->
        <CPaginationItem
          v-for="p in visiblePages"
          :key="p"
          :active="p === page"
          @click="page = p"
        >
          {{ p }}
        </CPaginationItem>

        <!-- Next page -->
        <CPaginationItem :disabled="page === totalPages" @click="page++">&gt;</CPaginationItem>

        <!-- Last page -->
        <CPaginationItem :disabled="page === totalPages" @click="page = totalPages">&raquo;</CPaginationItem>
      </CPagination>
    </CCardFooter>
  </CCard>
</template>
