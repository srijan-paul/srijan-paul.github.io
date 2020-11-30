<template>
  <section id="app">
    <div class="wrapper">
      <DashBoard />
      <hr />
      <CardList />
      <hr />
      <Tools />
      <hr />
      <ProjectList />
    </div>
  </section>
</template>

<script>
import DashBoard from "@/components/DashBoard";
import ProjectList from "./components/ProjectList";
import CardList from "@/components/CardList";
import Tools from "@/components/Tools";

export default {
  name: "App",
  components: {
    DashBoard,
    ProjectList,
    CardList,
    Tools,
  },
};
</script>

<style>
@import url("https://fonts.googleapis.com/css2?family=Roboto&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Montserrat&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Fira+Code&display=swap");

:root {
  --hlColor: #2e86de;
  --bgColor: #ffffff;
  --fgColor: #333333;
}

body {
  margin: 0;
  padding: 0;
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
  font-family: "Fira Code", sans-serif;
  /* text-decoration: underline #2980b9; */
}

.section-title {
  font-family: "Roboto", sans-serif;
  font-weight: 700;
}

#app {
  margin: 5% auto 8%;
}

.wrapper {
  width: 60%;
  margin: auto;
  font-size: 1.7rem;
  text-align: center;
}

@media only screen and (max-width: 600px) {
  .wrapper {
    width: 90%;
  }
}
</style>
