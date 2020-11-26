<template>
  <div class="project-card">
    <span class="card-title">{{ title }}</span>
    <p><slot>Project description missing</slot></p>
    <Button name="Github" :url="link" />
  </div>
</template>

<script>
import Button from "@/components/Button";

export default {
  props: {
    title: {
      type: String,
      required: true,
    },
    link: String,
  },
  components: {
    Button,
  },
};
</script>

<style scoped>
.project-card {
  text-align: left;
  border-radius: 2%;
  padding: 3%;
  box-shadow: -1px 3px 23px -11px rgba(179, 168, 175, 1);
  margin-bottom: 5%;
  background-color: #ffffff;
}

.card-title {
  font-weight: 700;
  font-size: 2.5rem;
}
</style>
