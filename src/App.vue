<template>
  <section id="app">
    <div class="wrapper">
      <DashBoard />
      <br />
      <ProjectList />
    </div>
  </section>
</template>

<script>
import DashBoard from "@/components/DashBoard";
import ProjectList from "./components/ProjectList.vue";

export default {
  name: "App",
  components: {
    DashBoard,
    ProjectList,
  },
};
</script>

<style>
@import url("https://fonts.googleapis.com/css2?family=Roboto&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Montserrat&display=swap");

:root {
  --hlColor: #2e86de;
  --bgColor: #ffffff;
  --fgColor: #333333;
}

body {
  font-family: "Montserrat", sans-serif;
  color: var(--fgColor);
  background-color: var(--bgColor);
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-family: "Roboto", sans-serif;
}

.section-title {
  font-family: "Roboto", sans-serif;
  font-weight: 700;
}

#app {
  margin-top: 5%;
}

.wrapper {
  width: 60%;
  margin: auto;
  font-size: 1.7rem;
  text-align: center;
}
</style>
