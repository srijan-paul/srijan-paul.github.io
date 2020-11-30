<template>
  <div class="card-list">
    <h3>Interests</h3>
    <div class="cards">
      <Card title="Compiler design" icon="compiler">
        My favorite CS subject. I've written several toy programming languages
        over the course of my learning. More recently, I've been developing a
        more "serious" programming language project that you can find below, or
        on my github. My favorite tools for this are C/C++. I used to be a
        stickler for C but lately I've come to find C++'s improvements
        worthwhile, safe and time saving
      </Card>

      <Card title="Web" icon="web">
        I tinker with web dev stuff very often. Mostly in JS/TS. VueJS is my
        tool of choice for front ends, It hits the perfect balance between
        sufficient tooling and minimalism. If I'm not using Vue then I'm most
        likely just using vanilla HTML and CSS/SASS. For the backend- express,
        NodeJS and Firebase usually satisfy my needs.
      </Card>

      <Card title="Gamedev" icon="game">
        I love seeing the kind of creative Indie games that studios and
        individuals come up with. And following from there, I also enjoy game
        development as a hobby. I've tried a bit of several toolchains and
        engines, but my favorite will always be <b>LOVE2D</b>. The fact that it
        uses Lua as a scripting language is ofcourse a big bonus on top of it's
        design :)
      </Card>
    </div>
  </div>
</template>

<script>
import Card from "@/components/Card";
export default {
  components: { Card },
};
</script>

<style scoped>
.cards {
  margin-top: 5%;
  display: grid;
  gap: 4rem;
  grid-template-columns: 1fr 1fr;
}

@media only screen and (max-width: 1000px) {
  .cards {
    grid-template-columns: 1fr;
  }
}
</style>
