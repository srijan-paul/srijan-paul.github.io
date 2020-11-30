<template>
  <div class="card">
    <img :src="require(`../assets/${icon}.svg`)" width="54" height="54" />
    <h6 class="card-title">{{ title }}</h6>
    <p class="card-text">
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
    icon: String,
  },
};
</script>

<style scoped>
.card {
  text-align: center;
  border-radius: 4%;
  padding: 3%;
  box-shadow: -1px 3px 23px -11px rgba(179, 168, 175, 1);
  margin-bottom: 5%;
  background-color: #ffffff;
  font-size: 15px;
  padding: 4%;
}

.card-text {
  padding: 0 4% 0 4%;
  text-align: justify;
}
</style>
