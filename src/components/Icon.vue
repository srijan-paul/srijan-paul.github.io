<template>
  <div style="display:inline-block" class="icon">
    <div class="tooltip">
      <img
        :src="require(`../assets/${name}`)"
        :width="size"
        :height="size"
        :alt="alt"
      />
      <span class="tooltip-text">{{ text }}</span>
    </div>
  </div>
</template>

<script>
export default {
  props: {
    name: String,
    size: String,
    alt: String,
    text: String,
  },
};
</script>

<style scoped>
.tooltip {
  position: relative;
  display: inline-block;
}

.tooltip .tooltip-text {
  position: absolute;
  text-align: center;
  bottom: -10%;
  left: 50%;
  margin-right: -40px;
  color: white;
  background-color: rgba(0, 0, 0, 0.7);
  border-radius: 4px;
  padding: 1px 4px 1px 4px;
  visibility: hidden;
  z-index: 1;
}

.tooltip:hover .tooltip-text {
  visibility: visible;
}
</style>
