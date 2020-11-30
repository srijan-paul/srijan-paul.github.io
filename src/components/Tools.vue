<template>
  <div class="tools-list">
    <h3>Stuff I work with</h3>

    <div class="tool-title">Languages</div>
    <div class="tool-icons">
      <Icon name="cpp.svg" size="54" text="C++" />
      <Icon name="lua.svg" size="54" text="Lua" />
      <Icon name="typescript.svg" size="54" text="Typescript" />
      <Icon name="javascript.svg" size="54" text="Javascript " />
      <!-- credit: https://freeicons.io/vector-and-svg-logos-icons-15/python-icon-8948 -->
      <Icon name="python.svg" size="54" text="Python" />
    </div>

    <div class="tool-title">Frameworks / Services</div>
    <div class="tool-icons">
      <Icon name="vue.svg" size="54" text="Vue" />
      <Icon name="nodejs.svg" size="54" text="NodeJS" />
      <Icon name="express.svg" size="54" text="Express" />
      <Icon name="love2d.png" size="54" text="Love2D" />
      <!-- credit: https://freeicons.io/vector-and-svg-logos-icons-17/sass-icon-9036 -->
      <Icon name="sass.svg" size="54" text="Sass" />
      <!-- credit: https://freeicons.io/vector-and-svg-logos-icons-08/firebase-icon-icon -->
      <Icon name="firebase.svg" size="54" text="Firebase" />
    </div>
  </div>
</template>

<script>
import Icon from "@/components/Icon";
export default {
  components: { Icon },
};
</script>

<style scoped>
.tools-list .icon {
  margin: 10px;
}

.tool-title {
  font-size: 2rem;
  margin-bottom: 2%;
  color: #999;
}
</style>
