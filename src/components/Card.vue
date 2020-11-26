<template>
  <div class="card">
    <h1 class="card-title">{{ title }}</h1>
    <p>
      <slot>
        Card body missing.
      </slot>
    </p>
  </div>
</template>

<script>
export default {
  props: {
    title: String,
  },
};
</script>

<style scoped></style>
