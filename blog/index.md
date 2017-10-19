---
layout: full-width
title: Blog
---

  <h1 class="content-listing-header sans">Usually R snippets, with explanation</h1>
  <p>Many of these are from <a href="http://github.com/jflournoy/misc-r-projects/">this github repo</a>. I also post over at the <a href='http://blogs.uoregon.edu/rclub/author/flournoy-2/'>R Club blog</a>.</p>
  <ul class="content-listing ">
    {% for post in site.posts %}      
        <li class="listing">
          <hr class="slender">
          <a href="{{ post.url | prepend: site.baseurl }}"><h3 class="contrast">{{ post.title }}</h3></a>
          <br><span class="smaller">{{ post.date | date: "%B %-d, %Y" }}</span>  <br/>
          <div>{{ post.excerpt }}</div> 
        </li>
    {% endfor %}
  </ul>


