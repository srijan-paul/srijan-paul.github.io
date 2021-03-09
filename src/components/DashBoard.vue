<template>
  <div id="dashboard" class="container">
    <img src="@/assets/me.png" alt="Me..." class="rounded-img" />
    <h1 class="title">Srijan Paul</h1>

    <MediaIcons />

    <p class="about">
      Hi, I'm a 2nd year college undergrad studying Computer Science. Also a
      programmer and a hobbyist gamedev. I like compiler design and
      occassionally tinker with web stuff.
    </p>

    <div class="button-list">
      <Button name="Resume" />
      <Button name="Blog" url="https://srijan-paul.github.io/blog" />
    </div>
  </div>
</template>

<script>
import Button from "@/components/Button.vue";
import MediaIcons from "@/components/MediaIcons.vue";

export default {
  name: "DashBoard",
  components: { Button, MediaIcons },
};
</script>

<style scoped>

.title {
  font-family: 'Inconsolata';
  text-decoration: underline var(--hlColor);
  text-decoration-thickness: 8%;
}

.rounded-img {
  border-radius: 50%;
  width: 30%;
  height: auto;
}

.rounded-img img {
  object-fit: contain;
}

h1 {
  font-weight: 600;
}

#dashboard {
  width: 90%;
  font-size: 2.2rem;
  text-align: center;
}

.button-list {
  width: 50%;
  margin: auto;
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  padding-top: 1%;
}

.button-list > .button {
  margin: 2%;
}

.about {
  margin-top: 4%;
  text-align: center;
  font-size: 2rem;
}

@media only screen and (max-width: 600px) {
  .about {
    font-size: 1.5rem;
  }
}
</style>
