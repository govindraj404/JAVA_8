<script setup>
import { ref } from 'vue'
import CommonSmartTable from './components/CommonSmartTable.vue'

// Mock test data
const rows = ref([
  { id: 1, name: 'John Doe', email: 'john@example.com' },
  { id: 2, name: 'Jane Smith', email: 'jane@example.com' },
  { id: 3, name: 'Alice Brown', email: 'alice@example.com' },
  { id: 4, name: 'Bob Johnson', email: 'bob@example.com' },
  { id: 5, name: 'Chris White', email: 'chris@example.com' },
  { id: 6, name: 'Daniel Black', email: 'daniel@example.com' },
  { id: 7, name: 'Eva Green', email: 'eva@example.com' },
  { id: 8, name: 'Frank Grey', email: 'frank@example.com' },
  { id: 9, name: 'Grace Blue', email: 'grace@example.com' },
  { id: 10, name: 'Harry Brown', email: 'harry@example.com' },
  { id: 11, name: 'Ivy Red', email: 'ivy@example.com' },
  { id: 12, name: 'Jack Gold', email: 'jack@example.com' },
])

// Handle selection change event
function onSelection(selected) {
  console.log('Selected rows:', selected)
}
</script>

<template>
  <div class="p-4">
    <h3>🔹 Full Options Enabled</h3>
    <CommonSmartTable
      :columns="[
        { key: 'id', label: 'ID' },
        { key: 'name', label: 'Name' },
        { key: 'email', label: 'Email' }
      ]"
      :data="rows"
      selectable
      row-key-field="id"
      paginated
      :page-size="5"
      @selection-change="onSelection"
    />

    <hr class="my-4" />

    <h3>🔹 Minimal Table (No Pagination, No Selection)</h3>
    <CommonSmartTable
      :columns="[
        { key: 'id', label: 'ID' },
        { key: 'name', label: 'Name' }
      ]"
      :data="rows"
      :paginated="false"
    />
  </div>
</template>
