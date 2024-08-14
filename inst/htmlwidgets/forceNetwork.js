HTMLWidgets.widget({

  name: "forceNetwork",

  type: "output",

  initialize: function(el, width, height) {
    d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height]);
        
    return d3.forceSimulation();
  },

  resize: function(el, width, height, force) {
    d3.select(el).select("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height]);
        
    force.force("center", d3.forceCenter(width / 2, height / 2))
        .restart();
  },

  renderValue: function(el, x, force) {
    function nodeSize(d) {
      if(options.nodesize){
        return eval(options.radiusCalculation);
      } else {
        return 6;
      }
    }

    var options = x.options;
    var links = HTMLWidgets.dataframeToD3(x.links);
    var nodes = HTMLWidgets.dataframeToD3(x.nodes);
    
    nodes.forEach(node => {
        node.fx = node.x + 350;
        node.fy = node.y + 50;
    });

    var linkedByIndex = {};
    links.forEach(function(d) {
      linkedByIndex[d.source + "," + d.target] = 1;
      linkedByIndex[d.target + "," + d.source] = 1;
    });
    function neighboring(a, b) {
      return linkedByIndex[a.index + "," + b.index];
    }

    var width = el.offsetWidth;
    var height = el.offsetHeight;

    var color = eval(options.colourScale);
    var shape = d3.scaleOrdinal(d3.symbols);

    var zoom = d3.zoom();

    force
      .nodes(d3.values(nodes))
      .force("link", d3.forceLink(links).distance(options.linkDistance))
      .force("center", d3.forceCenter(width / 2, height / 2))
      .force("charge", d3.forceManyBody().strength(options.charge))
      .on("tick", tick);

    force.alpha(1).restart();

    var drag = d3.drag()
      .on("start", dragstart)
      .on("drag", dragged)
      .on("end", dragended);

    function dragstart(d) {
      if (!d3.event.active) force.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }
    function dragged(d) {
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }
    function dragended(d) {
      if (!d3.event.active) force.alphaTarget(0);
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }

    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();
    svg = svg
        .append("g").attr("class","zoom-layer")
        .append("g");

    if (options.zoom) {
      function redraw() {
        d3.select(el).select(".zoom-layer")
          .attr("transform", d3.event.transform);
      }
      zoom.on("zoom", redraw);

      d3.select(el).select("svg")
        .attr("pointer-events", "all")
        .call(zoom);
    } else {
      zoom.on("zoom", null);
    }
    
    var tooltip = svg.append('div')
         .attr('class','tooltip')
         .style("position", "absolute")
         .style("z-index", "10")
         .attr('width', 200)
         .attr('height', 200)
         .attr('id', 'tooltip')
         .text("a simple tooltip");

    var link = svg.selectAll(".link")
      .data(links)
      .enter().append("line")
      .attr("class", "link")
      .style("stroke", function(d) { return d.colour; })
      .style("opacity", 1)
      .style("stroke-width", eval("(" + options.linkWidth + ")"))
      .on("click", click)
      .on("mouseover", function(d) { 
          d3.select(this)
            .style("opacity", 1)
            .style("stroke-width", 5);
      })
      .on("mouseout", function(d) {
          d3.select(this)
            .style("opacity", 1)
            .style("stroke-width", eval("(" + options.linkWidth + ")"));
      });
      
    var formatNumber = d3.format(",.0f"),
        format = function(d) { 
            if (typeof d === "string") return d;
            return formatNumber(d); 
        };
      
    link.append("title")
      .append("foreignObject")
      .append("xhtml:body")
      .html(function(d) { return "<pre>" + d.source.name + " \u2192 " + d.target.name +"</pre>" + 
          "\n\nEffect: " + d.effect +
          "\nArea: " + d.area +
          "\nConfidence: " + d.confidence +
          "\n\nReference(s): \n" + d.reference;
      });

    if (options.arrows) {
      link.style("marker-end",  function(d) { return "url(#arrow-" + d.colour + ")"; });

      var linkColoursArr = d3.nest().key(function(d) { return d.colour; }).entries(links);

      svg.append("defs").selectAll("marker")
          .data(linkColoursArr)
          .enter().append("marker")
            .attr("id", function(d) { return "arrow-" + d.key; })
            .attr("viewBox", "0, -5, 10, 10")
            .attr("refX", 0)
            .attr("markerWidth", 4)
            .attr("markerHeight", 4)
            .attr("orient", "auto")
            .style("fill", "context-fill")
            .style("fill", function(d) { return d.key; })
            .style("opacity", options.opacity)
          .append("path")
            .attr("d", "M0,-5 L10,0 L0,5");
    }
    
    var node = svg.selectAll(".node")
      .data(force.nodes())
      .enter().append("g")
      .attr("class", "node")
      .style("fill", function(d) { return color(d.group); })
      .style("opacity", options.opacity)
      .on("mouseover", mouseover)
      .on("mouseout", mouseout)
      .call(drag);

    node.append("path")
      .attr("d", d3.symbol()
        .type(function(d) { return shape(d.group); })
        .size(function(d) { return nodeSize(d) * nodeSize(d) * Math.PI; })
      )
      .style("stroke", "#fff")
      .style("opacity", options.opacity)
      .style("stroke-width", "1.5px");

    node.append("svg:text")
      .attr("class", "nodetext")
      .attr("dx", function(d) { return d.dx; })
      .attr("dy", function(d) { return d.dy; })
      .attr("transform", function(d) {
          return "rotate(" + d.rotation + ")"; 
       })
      .text(function(d) { return d.name; })
      .style("text-anchor", function(d) { return d.text_anchor; })
      .style("font", options.fontSize + "px " + options.fontFamily)
      .style("opacity", options.opacityNoHover)
      .style("pointer-events", "none")
      .style("fill", "black");

    function tick() {
      node.attr("transform", function(d) {
        if(options.bounded){
            d.x = Math.max(nodeSize(d), Math.min(width - nodeSize(d), d.x));
            d.y = Math.max(nodeSize(d), Math.min(height - nodeSize(d), d.y));
        }
        return "translate(" + d.x + "," + d.y + ")";
      });

      function idx(d, type) {
        var linkWidthFunc = eval("(" + options.linkWidth + ")");
        var a = d.target.x - d.source.x;
        var b = d.target.y - d.source.y;
        var c = Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2));
        if (type == "x1") return (d.source.x + ((nodeSize(d.source) * a) / c));
        if (type == "y1") return (d.source.y + ((nodeSize(d.source) * b) / c));
        if (options.arrows) {
          if (type == "x2") return (d.target.x - ((((5 * linkWidthFunc(d)) + nodeSize(d.target)) * a) / c));
          if (type == "y2") return (d.target.y - ((((5 * linkWidthFunc(d)) + nodeSize(d.target)) * b) / c));
        } else {
          if (type == "x2") return (d.target.x - ((nodeSize(d.target) * a) / c));
          if (type == "y2") return (d.target.y - ((nodeSize(d.target) * b) / c));
        }
      }

      link
        .attr("x1", function(d) { return idx(d, "x1"); })
        .attr("y1", function(d) { return idx(d, "y1"); })
        .attr("x2", function(d) { return idx(d, "x2"); })
        .attr("y2", function(d) { return idx(d, "y2"); });
    }

    function mouseover(d) {
      var unfocusDivisor = 4;

      link.transition().duration(0)
        .style("opacity", function(l) { return d != l.source && d != l.target ? 0 : +options.opacity; });

      node.transition().duration(0)
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });
        
      d3.select(this).select("path").transition()
        .duration(0)
        .attr("d", d3.symbol()
          .type(function(d) { return shape(d.group); })
          .size(function(d) { return nodeSize(d) * nodeSize(d) * Math.PI; })
        );
      d3.select(this).select("text").transition()
        .duration(0)
        .style("stroke-width", ".5px")
        .style("font", options.clickTextSize + "px ")
        .style("opacity", 1);
    }

    function mouseout() {
      node.style("opacity", +options.opacity);
      link.style("opacity", +options.opacity); 

      d3.select(this).select("path").transition()
        .duration(0)
        .attr("d", d3.symbol()
          .type(function(d) { return shape(d.group); })
          .size(function(d) { return nodeSize(d) * nodeSize(d) * Math.PI; })
        );
      d3.select(this).select("text").transition()
        .duration(0)
        .style("font", options.fontSize + "px ")
        .style("opacity", options.opacityNoHover);
    }

    function click(d) {
      return eval(options.clickAction);
    }

if(options.legend){
    var legendRectSize = 18;
    var legendShapeSize = 64; // This controls the size of the shape. Decrease this value to make shapes smaller.
    var legendSpacing = 4;
    var legend = d3.select(el).select('svg').selectAll('.legend')
      .data(color.domain())
      .enter()
      .append('g')
      .attr('class', 'legend')
      .attr('transform', function(d, i) {
        var height = legendRectSize + legendSpacing;
        var offset =  height * color.domain().length / 2;
        var horz = legendRectSize;
        var vert = i * height + 4;
        return 'translate(' + horz + ',' + vert + ')';
      });

    legend.append('path')
      .attr('d', d3.symbol()
        .type(function(d) { return shape(d); })
        .size(legendShapeSize) // Use the new legendShapeSize here
      )
      .attr('transform', function(d) {
        return 'translate(' + (legendRectSize / 2) + ',' + (legendRectSize / 2) + ')';
      })
      .style('fill', color)
      .style('stroke', color);

    legend.append('text')
      .attr('x', legendRectSize + legendSpacing)
      .attr('y', legendRectSize / 2)
      .attr('dy', '.35em') // Vertically center the text
      .text(function(d) { return d; });
}

    d3.select(el).selectAll('text').style('font-family', options.fontFamily);
  },
});