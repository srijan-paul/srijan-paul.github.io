<template>
  <div class="button" @click="handleClick">
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
    url: String,
  },

  methods: {
    handleClick() {
      if (this.url) window.open(this.url, "_blank");
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
