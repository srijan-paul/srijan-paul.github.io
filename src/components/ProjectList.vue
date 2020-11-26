<template>
  <div class="projects">
    <h2 class="section-title">Projects</h2>
    <div class="project-list">
      <ProjectCard title="Ave" link="https://github.com/srijan-paul/AveTS">
        A statically type checked programming language that compiles to
        Javascript. It is very similar to typescript, except it has a minimal
        whitespace sensitive syntax like python.
      </ProjectCard>

      <ProjectCard
        title="Mad Carbon"
        link="https://github.com/srijan-paul/bullet_hell"
      >
        A retro bullet hell game written in Lua with the Love2D game framework.
        The player explores a dungeon filled with monsters, critters and
        treasures to reach the orb of truth. Offers a variety of weapons, items
        and a level editor that can be used to craft levels.
      </ProjectCard>

      <ProjectCard
        title="JSONC"
        link="https://github.com/srijan-paul/json-parser"
      >
        A fast and simple JSON parser written in C. JSONC makes it extremely
        simple to load JSON data from files and strings into your C and C++
        projects.
      </ProjectCard>

      <ProjectCard title="FoxVM" link="https://github.com/srijan-paul/FoxVM">
        A fast and straightforward register based virtual machine written in
        C++.
      </ProjectCard>
    </div>
  </div>
</template>

<script>
import ProjectCard from "@/components/ProjectCard";
export default { components: { ProjectCard } };
</script>

<style scoped>
.projects {
  margin-top: 4%;
}
</style>
