<template>
  <div>
    <h2>My Table Example</h2>

    <SmartTable
      :columns="columns"
      :data="rows"
      :total="rows.length"
      :serverMode="false"
      rowKeyField="id"
      selectable
      editable
      @update:page="onPageChange"
      @update:pageSize="onPageSizeChange"
      @sort="onSort"
      @filter="onFilter"
      @selection-change="onSelectionChange"
      @edit="onEdit"
      @delete-row="onDelete"
    >
      <!-- Optional actions slot -->
      <template #actions="{ row }">
        <button @click="alert(`Viewing ${row.name}`)">View</button>
      </template>
    </SmartTable>
  </div>
</template>

<script setup>
import SmartTable from '@/components/SmartTable.vue'

const columns = [
  { key: 'id', label: 'ID', sortable: true },
  { key: 'name', label: 'Name', sortable: true, editable: true },
  { key: 'age', label: 'Age', sortable: true, editable: true },
  { key: 'email', label: 'Email', sortable: true }
]

const rows = [
  { id: 1, name: 'John Doe', age: 28, email: 'john@example.com' },
  { id: 2, name: 'Jane Smith', age: 34, email: 'jane@example.com' },
  { id: 3, name: 'Michael Brown', age: 41, email: 'michael@example.com' }
]

// event handlers
function onPageChange(page) {
  console.log('Page changed:', page)
}

function onPageSizeChange(size) {
  console.log('Page size changed:', size)
}

function onSort(sort) {
  console.log('Sort changed:', sort)
}

function onFilter(query) {
  console.log('Filter query:', query)
}

function onSelectionChange(selectedKeys) {
  console.log('Selected rows:', selectedKeys)
}

function onEdit({ rowKey, updates }) {
  console.log('Row edit:', rowKey, updates)
}

function onDelete(row) {
  console.log('Deleting row:', row)
}
</script>
