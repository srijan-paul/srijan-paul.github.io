<template>
  <div class="button">
    {{ name }}
  </div>
</template>

<script>
export default {
  name: "Button",
  props: {
    name: {
      type: String,
      required: true,
    },
  },
};
</script>

<style scoped>
.button:hover {
  color: white;
  background-color: var(--hlColor);
  border-color: var(--hlColor);
  transition-duration: 300ms;
}
</style>
