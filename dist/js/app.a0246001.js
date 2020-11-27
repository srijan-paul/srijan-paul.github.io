(function(t){function e(e){for(var r,s,o=e[0],c=e[1],l=e[2],p=0,f=[];p<o.length;p++)s=o[p],Object.prototype.hasOwnProperty.call(a,s)&&a[s]&&f.push(a[s][0]),a[s]=0;for(r in c)Object.prototype.hasOwnProperty.call(c,r)&&(t[r]=c[r]);u&&u(e);while(f.length)f.shift()();return i.push.apply(i,l||[]),n()}function n(){for(var t,e=0;e<i.length;e++){for(var n=i[e],r=!0,o=1;o<n.length;o++){var c=n[o];0!==a[c]&&(r=!1)}r&&(i.splice(e--,1),t=s(s.s=n[0]))}return t}var r={},a={app:0},i=[];function s(e){if(r[e])return r[e].exports;var n=r[e]={i:e,l:!1,exports:{}};return t[e].call(n.exports,n,n.exports,s),n.l=!0,n.exports}s.m=t,s.c=r,s.d=function(t,e,n){s.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:n})},s.r=function(t){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},s.t=function(t,e){if(1&e&&(t=s(t)),8&e)return t;if(4&e&&"object"===typeof t&&t&&t.__esModule)return t;var n=Object.create(null);if(s.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var r in t)s.d(n,r,function(e){return t[e]}.bind(null,r));return n},s.n=function(t){var e=t&&t.__esModule?function(){return t["default"]}:function(){return t};return s.d(e,"a",e),e},s.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},s.p="/srijan-paul.github.io/";var o=window["webpackJsonp"]=window["webpackJsonp"]||[],c=o.push.bind(o);o.push=e,o=o.slice();for(var l=0;l<o.length;l++)e(o[l]);var u=c;i.push([0,"chunk-vendors"]),n()})({0:function(t,e,n){t.exports=n("56d7")},"022b":function(t,e,n){},"034f":function(t,e,n){"use strict";n("85ec")},"0617":function(t,e,n){"use strict";n("5a88")},"0859":function(t,e,n){"use strict";n("f34e")},"387e":function(t,e,n){t.exports=n.p+"img/github.617870e4.svg"},"56d7":function(t,e,n){"use strict";n.r(e);n("e260"),n("e6cf"),n("cca6"),n("a79d");var r=n("2b0e"),a=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("section",{attrs:{id:"app"}},[n("div",{staticClass:"wrapper"},[n("DashBoard"),n("br"),n("ProjectList")],1)])},i=[],s=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",{staticClass:"container",attrs:{id:"dashboard"}},[r("img",{staticClass:"rounded-img",attrs:{src:n("9f2c"),alt:"Me.. not really."}}),r("h1",[t._v("Srijan Paul")]),r("MediaIcons"),r("p",{staticClass:"about"},[t._v(" Hi, I'm a 2nd year college undergrad studying Computer Science. Also a programmer and a hobbyist gamedev. I like compiler design and occassionally tinker with web stuff. ")]),r("div",{staticClass:"button-list"},[r("Button",{attrs:{name:"Resume"}}),r("Button",{attrs:{name:"Blog",url:"https://srijan-paul.github.io/blog"}})],1)],1)},o=[],c=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("div",{staticClass:"button",on:{click:t.handleClick}},[t._v(" "+t._s(t.name)+" ")])},l=[],u={name:"Button",props:{name:{type:String,required:!0},url:String},methods:{handleClick:function(){this.url&&window.open(this.url,"_blank")}}},p=u,f=(n("0617"),n("2877")),d=Object(f["a"])(p,c,l,!1,null,"1f6aac20",null),h=d.exports,m=function(){var t=this,e=t.$createElement;t._self._c;return t._m(0)},g=[function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",{staticClass:"media-icons"},[r("a",{staticClass:"social-icon",attrs:{href:"https://github.com/srijan-paul",target:"_blank"}},[r("img",{attrs:{height:"24",width:"24",src:n("387e")}})]),r("a",{staticClass:"social-icon",attrs:{href:"https://twitter.com/_injuly",target:"_blank"}},[r("img",{attrs:{height:"24",width:"24",src:n("a3df")}})]),r("a",{staticClass:"social-icon",attrs:{href:"https://www.linkedin.com/in/srijan-paul-a731901aa/",target:"_blank"}},[r("img",{attrs:{height:"24",width:"24",src:n("953d")}})])])}],b=(n("0859"),{}),v=Object(f["a"])(b,m,g,!1,null,"f66ce626",null),_=v.exports,j={name:"DashBoard",components:{Button:h,MediaIcons:_}},y=j,w=(n("73e7"),Object(f["a"])(y,s,o,!1,null,"1516b4ae",null)),C=w.exports,k=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("div",{staticClass:"projects"},[n("h2",{staticClass:"section-title"},[t._v("Projects")]),n("div",{staticClass:"project-list"},[n("ProjectCard",{attrs:{title:"Ave",link:"https://github.com/srijan-paul/AveTS"}},[t._v(" A statically type checked programming language that compiles to Javascript. It is very similar to typescript, except it has a minimal whitespace sensitive syntax like python. ")]),n("ProjectCard",{attrs:{title:"FoxVM",link:"https://github.com/srijan-paul/FoxVM"}},[t._v(" A fast and straightforward register based virtual machine written in C++. ")]),n("ProjectCard",{attrs:{title:"Mad Carbon",link:"https://github.com/srijan-paul/bullet_hell"}},[t._v(" A retro bullet hell game written in Lua with the Love2D game framework. The player explores a dungeon filled with monsters, critters and treasures to reach the orb of truth. Offers a variety of weapons, items and a level editor that can be used to craft levels. ")]),n("ProjectCard",{attrs:{title:"JSONC",link:"https://github.com/srijan-paul/json-parser"}},[t._v(" A fast and simple JSON parser written in C. JSONC makes it extremely simple to load JSON data from files and strings into your C and C++ projects. ")])],1)])},x=[],O=function(){var t=this,e=t.$createElement,n=t._self._c||e;return n("div",{staticClass:"project-card"},[n("span",{staticClass:"card-title"},[t._v(t._s(t.title))]),n("p",[t._t("default",[t._v("Project description missing")])],2),n("Button",{attrs:{name:"Github",url:t.link}})],1)},P=[],S={props:{title:{type:String,required:!0},link:String},components:{Button:h}},B=S,M=(n("eefe"),Object(f["a"])(B,O,P,!1,null,"63688f02",null)),A=M.exports,$={components:{ProjectCard:A}},E=$,J=(n("6065"),Object(f["a"])(E,k,x,!1,null,"2670b26a",null)),I=J.exports,T={name:"App",components:{DashBoard:C,ProjectList:I}},D=T,L=(n("034f"),Object(f["a"])(D,a,i,!1,null,null,null)),N=L.exports;r["a"].config.productionTip=!1,new r["a"]({render:function(t){return t(N)}}).$mount("#app")},"5a88":function(t,e,n){},6065:function(t,e,n){"use strict";n("98e4")},"73e7":function(t,e,n){"use strict";n("7e48")},"7e48":function(t,e,n){},"85ec":function(t,e,n){},"953d":function(t,e,n){t.exports=n.p+"img/linkedin.619231ff.svg"},"98e4":function(t,e,n){},"9f2c":function(t,e,n){t.exports=n.p+"img/me.991568dc.png"},a3df:function(t,e,n){t.exports=n.p+"img/twitter.20d9f515.svg"},eefe:function(t,e,n){"use strict";n("022b")},f34e:function(t,e,n){}});
//# sourceMappingURL=app.a0246001.js.map