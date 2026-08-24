/* Trace Ledger — client-side expand/collapse, details, collapse-all.
   Operand drill-down injects HTML without rebuilding the tree. */
(function () {
  "use strict";

  if (window.__tlTraceLedgerBound) {
    return;
  }
  window.__tlTraceLedgerBound = true;

  var pendingScroll = null;

  function closestNode(el) {
    return el && el.closest ? el.closest(".tl-node") : null;
  }

  function scrollContainerFor(el) {
    var cur = el;
    while (cur && cur !== document.body) {
      var style = window.getComputedStyle(cur);
      if (
        (style.overflowY === "auto" || style.overflowY === "scroll") &&
        cur.scrollHeight > cur.clientHeight
      ) {
        return cur;
      }
      cur = cur.parentElement;
    }
    return document.documentElement;
  }

  function captureScroll(anchorEl) {
    var sc = scrollContainerFor(anchorEl || document.querySelector(".tl-root"));
    if (sc === document.documentElement) {
      return { el: sc, top: window.scrollY, left: window.scrollX };
    }
    return { el: sc, top: sc.scrollTop, left: sc.scrollLeft };
  }

  function restoreScroll(saved) {
    if (!saved) return;
    if (saved.el === document.documentElement) {
      window.scrollTo(saved.left, saved.top);
    } else {
      saved.el.scrollTop = saved.top;
      saved.el.scrollLeft = saved.left;
    }
  }

  function setCollapsed(node, collapsed) {
    if (!node) return;
    var chevron = node.querySelector(":scope > .tl-node-body .tl-chevron");
    if (collapsed) {
      node.classList.add("is-collapsed");
      if (chevron) {
        chevron.setAttribute("aria-expanded", "false");
        chevron.textContent = "\u25B8";
      }
    } else {
      node.classList.remove("is-collapsed");
      if (chevron) {
        chevron.setAttribute("aria-expanded", "true");
        chevron.textContent = "\u25BE";
      }
    }
  }

  function toggleCollapsed(node) {
    if (!node) return;
    setCollapsed(node, !node.classList.contains("is-collapsed"));
  }

  function setDetailsOpen(node, open) {
    if (!node) return;
    var body = node.querySelector(":scope > .tl-node-body");
    if (open) {
      node.classList.add("is-details-open");
      if (body) body.setAttribute("aria-expanded", "true");
    } else {
      node.classList.remove("is-details-open");
      if (body) body.setAttribute("aria-expanded", "false");
    }
  }

  function toggleDetails(node) {
    if (!node || node.classList.contains("tl-bridge")) return;
    setDetailsOpen(node, !node.classList.contains("is-details-open"));
  }

  function expandAncestors(node) {
    var cur = node ? node.parentElement : null;
    while (cur) {
      var parentNode = cur.closest ? cur.closest(".tl-node") : null;
      if (!parentNode || parentNode === node) break;
      setCollapsed(parentNode, false);
      cur = parentNode.parentElement;
    }
  }

  function activateStripCell(stripCell) {
    var strip = stripCell.closest(".tl-period-strip");
    if (strip) {
      strip.querySelectorAll(".tl-strip-cell-drilldown").forEach(function (cell) {
        cell.classList.remove("is-drilldown-active");
      });
    }
    stripCell.classList.add("is-drilldown-active");
  }

  function applyOperandDrilldown(payload) {
    if (!payload || !payload.step_id) return;

    var node = document.querySelector(
      '.tl-node[data-step-id="' + payload.step_id + '"]'
    );
    if (!node) return;

    expandAncestors(node);
    setCollapsed(node, false);
    setDetailsOpen(node, true);

    var slot = node.querySelector(".tl-operand-drilldown-slot");
    if (slot) {
      slot.innerHTML = payload.html || "";
    }

    if (payload.period) {
      node.querySelectorAll(".tl-strip-cell-drilldown").forEach(function (cell) {
        cell.classList.toggle(
          "is-drilldown-active",
          cell.getAttribute("data-period") === payload.period
        );
      });
    }

    restoreScroll(pendingScroll);
    pendingScroll = null;
  }

  function handleStripCellClick(stripCell, ev) {
    ev.preventDefault();
    ev.stopPropagation();

    pendingScroll = captureScroll(stripCell);

    var node = closestNode(stripCell);
    if (node) {
      expandAncestors(node);
      setCollapsed(node, false);
      setDetailsOpen(node, true);
    }
    activateStripCell(stripCell);

    if (window.Shiny && Shiny.setInputValue) {
      var drillInputId = stripCell.getAttribute("data-input-id");
      if (drillInputId) {
        Shiny.setInputValue(
          drillInputId,
          {
            step_id: stripCell.getAttribute("data-step-id"),
            country_id: stripCell.getAttribute("data-country-id"),
            indicator_code: stripCell.getAttribute("data-indicator-code"),
            frequency: stripCell.getAttribute("data-frequency"),
            period: stripCell.getAttribute("data-period"),
            nonce: Date.now()
          },
          { priority: "event" }
        );
      }
    }
  }

  function collapseAll(root) {
    if (!root) return;
    root.querySelectorAll(".tl-node").forEach(function (node) {
      var hasKids = node.querySelector(":scope > .tl-children .tl-node");
      if (hasKids) setCollapsed(node, true);
      setDetailsOpen(node, false);
    });
    root.querySelectorAll(".tl-operand-drilldown-slot").forEach(function (slot) {
      slot.innerHTML = "";
    });
  }

  function expandDefault(root) {
    if (!root) return;
    root.querySelectorAll(".tl-node").forEach(function (node) {
      var level = parseInt(node.getAttribute("data-level") || "0", 10);
      var hasKids = !!node.querySelector(":scope > .tl-children .tl-node");
      if (!hasKids) return;
      setCollapsed(node, level >= 1);
    });
  }

  document.addEventListener("click", function (ev) {
    var t = ev.target;
    if (!t) return;

    if (t.closest && t.closest("[data-tl-collapse-all]")) {
      var host = document.querySelector(".tl-root");
      collapseAll(host);
      ev.preventDefault();
      return;
    }

    var stripCell = t.closest && t.closest(".tl-strip-cell-drilldown");
    if (stripCell) {
      handleStripCellClick(stripCell, ev);
      return;
    }

    if (t.classList && t.classList.contains("tl-bridge-show")) {
      ev.preventDefault();
      ev.stopPropagation();
      if (window.Shiny && Shiny.setInputValue) {
        var inputId = t.getAttribute("data-input-id") ||
          (document.querySelector("[data-tl-show-technical-input]") &&
            document
              .querySelector("[data-tl-show-technical-input]")
              .getAttribute("data-tl-show-technical-input"));
        if (inputId) {
          Shiny.setInputValue(inputId, Date.now(), { priority: "event" });
        }
      }
      return;
    }

    var chevronBtn = t.closest && t.closest(".tl-chevron");
    if (chevronBtn) {
      ev.preventDefault();
      ev.stopPropagation();
      toggleCollapsed(closestNode(chevronBtn));
      return;
    }

    var body = t.closest && t.closest(".tl-node-body");
    if (body) {
      var node = closestNode(body);
      if (node && !node.classList.contains("tl-bridge")) {
        toggleDetails(node);
      }
    }
  });

  document.addEventListener("keydown", function (ev) {
    var t = ev.target;
    if (!t || !t.classList || !t.classList.contains("tl-node-body")) return;
    if (ev.key === "Enter" || ev.key === " ") {
      ev.preventDefault();
      toggleDetails(closestNode(t));
    }
  });

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("tl_operand_drilldown", applyOperandDrilldown);
  } else {
    document.addEventListener("shiny:connected", function () {
      Shiny.addCustomMessageHandler("tl_operand_drilldown", applyOperandDrilldown);
    });
  }

  window.TraceLedger = {
    collapseAll: collapseAll,
    expandDefault: expandDefault,
    setCollapsed: setCollapsed
  };
})();
