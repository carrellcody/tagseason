// const tableHead = document.querySelector("#itemTable thead");
// const tableBody = document.querySelector("#itemTable tbody");
let currentPage = 1;
const rowsPerPage = 100;  
let parsedData = []; // global storage

// Sorting state
let sortColumn = null;
let sortDirection = "asc";




//Define columns that are visible for the draw table
const visibleColumnsDrawDeer = ["Tag", "Valid GMUs", "Drawn_out_level", "Chance_with_First_choice", "Chance_at_DOL",
  "Sex","Weapon", "Notes"];
  
//Define columns that are visible for the subtable in the draw results
const visibleColsSubDrawTable = ["Unit", "Bucks", "Antlerless", "Total Hunters","Percent Success","Category","Acres",
  "Acres Public", "percent_public", "Hunters Density Per Sq. Mile","Hunters Density Per Public Sq. Mile"]; 


  //,"Public_Acres","Public_Percent", "Notes"]; 
const visibleColumnsHarvestDeer = ["Unit",	"Category",	"Bucks",	"Antlerless",	"Total Harvest",	"Total Hunters",	
  "Percent Success",	"Total Rec. Days", "percent_public","Acres", "Acres Public","Hunters Density Per Sq. Mile","Hunters Density Per Public Sq. Mile"];

// Map CSV headers to display names
const headerLabelsDrawDeer = {
  "Tag": "Hunt Code",
  "Valid GMUs": "Valid Units",
  "Drawn_out_level": "Minimum points/level required to draw (Drawn out level)",
  "Chance_with_First_choice": "Chance to draw with indicated prefrence points at first choice",
  "Chance_at_DOL": "Chance at Drawn out level",
  "Sex": "Sex",
  "Weapon": "Weapon",
  "Notes": "Extra Info",
  "Total_Acres": "Total Acres Across All units",
  "Public_Acres": "Public Acres Across All units",
  "Public_Percent": "Percent Public Land Across All units",
  "Notes":"Notes"
};

const headerLabelsHarvestDeer = {
  "Unit": "Unit",
  "Category": "Harvest Category",
  "Bucks":"Bucks Killed",	
  "Antlerless" : "Antlerless Killed",
  "Total Harvest" : "Total Harvest",
  "Total Hunters" : "Total Hunters",
  "Percent Success" :"Success Rate",
  "Total Rec. Days" : "Total Rec Days"
  ,"percent_public" : "Public Land %",
  "Acres" : "Total Acres",
  "Acres Public" : "Public Acres"
};


const headerLabelsSubDraw = {
"Unit":"Unit",		
"Bucks": "Bucks Killed",	
"Antlerless": "Antlerless Killed",	
"Total Hunters" :"Total Hunters", 	
  "Percent Success":"Percent Success",	
  "Category":"Harvest Statistic Category",
   "Acres":"Acres", 
  "Acres Public":"Public Acres",
  "percent_public":"Percent Public Land",
  "Hunters Density Per Sq. Mile":"Hunter Density Per Sq. Mile (x1000)",
  "Hunters Density Per Public Sq. Mile":"Hunter Density Per Public Sq. Mile (x1000)"

}

// Toggle sort direction or switch column
function toggleSort(col) {
  if (sortColumn === col) {
    sortDirection = sortDirection === "asc" ? "desc" : "asc";
  } else {
    sortColumn = col;
    sortDirection = "asc";
  }
}

// Sort helper
function sortData(data) {
  if (!sortColumn) return data;

  return [...data].sort((a, b) => {
    let valA = a[sortColumn];
    let valB = b[sortColumn];

    // Try numeric compare
    let numA = parseFloat(valA);
    let numB = parseFloat(valB);
    if (!isNaN(numA) && !isNaN(numB)) {
      valA = numA;
      valB = numB;
    } else {
      valA = valA?.toString().toLowerCase() || "";
      valB = valB?.toString().toLowerCase() || "";
    }

    if (valA < valB) return sortDirection === "asc" ? -1 : 1;
    if (valA > valB) return sortDirection === "asc" ? 1 : -1;
    return 0;
  });
}


// Render function with pagination and mapping to pdf links for the deer DOA 
function renderDrawTable(
  data,
  page = 1,
  columns = visibleColumnsDrawDeer,
  tableEl = document.getElementById("itemTable")
) {
  const body = tableEl.querySelector("tbody");
  body.innerHTML = "";

  // Apply sorting before paginating
  const sortedData = sortData(data);
  const start = (page - 1) * rowsPerPage;
  const end = start + rowsPerPage;
  const rowsToShow = sortedData.slice(start, end);

  // Add links to the tag column to take you to the correct pdf
  const pdfUrl = encodeURIComponent(
    "https://cpw.widen.net/s/fm5zxrbhwz/postdrawrecapreport_deer-25_05102025_1540.pdf"
  );
  const pdfjsViewer = "https://mozilla.github.io/pdf.js/web/viewer.html";

  // Define which columns should be truncated
  const longTextCols = ["Notes"]; 

  // Create a fragment to batch DOM updates
  const fragment = document.createDocumentFragment();
rowsToShow.forEach(row => {
  const tr = document.createElement("tr");

  columns.forEach(col => {
    const td = document.createElement("td");

    // Step 3: add arrow indicator only for the first column
    if (col === "Tag") {
      const expandIcon = document.createElement("span");
      expandIcon.textContent = "▶"; // collapsed
      expandIcon.style.marginRight = "6px";
      td.appendChild(expandIcon);
    }

    // existing td population logic
    if (col === "Tag") {
      const code = row[col];
      const pageNum = huntCodeMap[code];
      if (pageNum) {
        const a = document.createElement("a");
        a.href = `${pdfjsViewer}?file=${pdfUrl}#page=${pageNum}`;
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        a.title = "Click to see full draw stats for this tag";
        a.textContent = code;
        td.appendChild(a);
      } else {
        td.appendChild(document.createTextNode(code ?? ""));
      }
    } else if (longTextCols.includes(col)) {
      const text = row[col] ?? "";
      td.textContent = text;
      td.title = text;
      td.classList.add("longtext");
    } else {
      td.textContent = row[col] ?? "";
    }

    tr.appendChild(td);
  });

  // Step 3: add click handler for expanding/collapsing sub-table
  tr.addEventListener("click", () => {
    const nextRow = tr.nextSibling;
    const firstTd = tr.querySelector("td span"); // the arrow span
    if (nextRow && nextRow.classList.contains("subrow")) {
      nextRow.remove();
      if (firstTd) firstTd.textContent = "▶"; // collapsed
    } else {
      if (firstTd) firstTd.textContent = "▼"; // expanded

      // create sub-table for units (same logic as before)
      const subTr = document.createElement("tr");
      subTr.classList.add("subrow");
      const subTd = document.createElement("td");
      subTd.colSpan = columns.length;
      subTd.style.padding = "8px 0";
      subTd.style.backgroundColor = "#f9f9f9";

      const subTable = document.createElement("table");
      subTable.classList.add("subtable");

      // create header
      const headerRow = document.createElement("tr");
      const unitColumns = Object.keys(unitAttributes[Object.keys(unitAttributes)[0]] || {});
      visibleColsSubDrawTable.forEach(vc => {
        const th = document.createElement("th");
        th.textContent = headerLabelsSubDraw[vc] || vc;
        headerRow.appendChild(th);
      });
      subTable.appendChild(headerRow);

      // add unit rows
      const validUnits = row["harvestunit"].split(",").map(u => u.trim());
      validUnits.forEach(unit => {
        if (unitAttributes[unit]) {
          const dataRow = document.createElement("tr");
          visibleColsSubDrawTable.forEach(uc => {
            const td = document.createElement("td");
            td.textContent = unitAttributes[unit][uc] ?? "";
            dataRow.appendChild(td);
          });
          subTable.appendChild(dataRow);
        }
      });

      subTd.appendChild(subTable);
      subTr.appendChild(subTd);
      tr.parentNode.insertBefore(subTr, tr.nextSibling);
    }
  });

  fragment.appendChild(tr);
});


  // Append all rows in one operation
  body.appendChild(fragment);

  // Pagination info
  const pageCount = Math.max(1, Math.ceil(data.length / rowsPerPage));
  const pageInfoEl = document.getElementById("pageInfo");
  if (pageInfoEl) pageInfoEl.textContent = `Page ${page} of ${pageCount}`;

  // Update row count
  const rowCountEl = document.getElementById("rowCount");
  if (rowCountEl) rowCountEl.textContent = `${data.length} tags match your criteria`;
}



//Render function for harvest table
function renderHarvestTable(data, page = 1) {
  const body = document.querySelector("#deerharvesttable tbody");
  body.innerHTML = "";

  const sorted = sortData(data);
  const rows = sorted.slice((page-1)*rowsPerPage, page*rowsPerPage);

  rows.forEach(row => {
    const tr = document.createElement("tr");
    visibleColumnsHarvestDeer.forEach(col => {
      const td = document.createElement("td");
      td.textContent = row[col] ?? "";
      tr.appendChild(td);
    });
    body.appendChild(tr);
  });
}




// Apply filters
function applyFilters(data) {
  const drawTable = document.getElementById("itemTable");          // table in Deer.html
  const harvestTable = document.getElementById("deerharvesttable"); // table in DeerHarvest.html

  // ---------- filters for Deer.html ----------
  function filterDrawPage(row, filterState) {
    let match = true;

    const { nameSearch, sexSelect, classSelect, ploSelect, rfwSelect, seasonSelect, ppLimit, noappsSelect } = filterState;

    // ---- Specific Unit search ----
    if (nameSearch) {
      const searchUnits = nameSearch.split(",").map(u => parseInt(u.trim(),10)).filter(n=>!isNaN(n));
      const rowUnits = String(row["Valid GMUs"]||"").split(",").map(u=>parseInt(u.trim(),10)).filter(n=>!isNaN(n));
      if (!searchUnits.some(su => rowUnits.includes(su))) match = false;
    }

    // Class filter
    if (classSelect.length && !classSelect.includes(row.Class)) match = false;

    // Sex filter with "Any" logic
    if (sexSelect.length && !sexSelect.includes("Any") && !sexSelect.includes(row.Sex)) match = false;

    // Season filter
    if (seasonSelect.length && !seasonSelect.includes("Any")) {
      const definedSeasons = ["A","M","O1R","O2R","O3R","O4R","E","L"];
      const SeasonWeapon = (row.SeasonWeapon || "Any").trim().toUpperCase();
      if (seasonSelect.includes("Other")) {
        if (definedSeasons.some(prefix => SeasonWeapon.includes(prefix))) match = false;
      } else if (!seasonSelect.some(sel => SeasonWeapon.includes(sel))) {
        match = false;
      }
    }

    // PP filter
    const dolValue = parseFloat(row.DOL);
    if (!isNaN(dolValue) && dolValue > ppLimit) match = false;

    // PLO filter
    if (ploSelect.includes("N") && row.PLO === "Yes") match = false;
    else if (ploSelect.includes("P") && row.PLO === "No") match = false;

    // RFW filter
    if (rfwSelect.includes("N") && row.RFW === "Yes") match = false;
    else if (rfwSelect.includes("R") && row.RFW === "No") match = false;

    // NoApps filter
    if (noappsSelect.includes("No") && row.NoApps === "Yes") match = false;

    return match;
  }

  // ---------- filters for DeerHarvest.html ----------
  function filterHarvestPage(row) {
  const harvestCats = Array.from(document.querySelectorAll("#harvestCheckboxes input[type=checkbox]:checked"))
    .map(cb => cb.value);

  const harvestUnit  = document.getElementById("harvestunit")?.value.trim();
  const minSuccess   = parseFloat(document.getElementById("minsr")?.value);
  const minPL       = parseFloat(document.getElementById("minpl")?.value);

  let match = true;

  // Category filter
  if (harvestCats.length && !harvestCats.includes("Any")) {
    if (!harvestCats.includes(row.Category)) {
      match = false;
    }
  }

  // Unit filter
  if (harvestUnit) {
     const units = harvestUnit.split(",").map(u => u.trim()).filter(u => u !== "");
    // Require exact match with at least one of the entered units
    if (!units.includes(String(row.Unit))) {
      match = false;
        }
      }

  // Success rate filter
  if (minSuccess){
    const rowRate = parseFloat(row["Percent Success"]);
    if (isNaN(rowRate) || rowRate < minSuccess) {
      match = false;
    }
  }

    // Percent Public Land filter
  if (minPL){
    const rowpublic = parseFloat(row["Percent Public Land"]);
    if (isNaN(rowpublic) || rowpublic < minPL) {
      match = false;
    }
  }

  return match;
}


  // ---------- gather filter input state once for draw table ----------
  if (drawTable) {
    const filterState = {
      nameSearch: document.getElementById("specunit")?.value.trim() || "",
      sexSelect: Array.from(document.querySelectorAll('input[name="sex"]:checked')).map(cb => cb.value),
      classSelect: Array.from(document.querySelectorAll('input[name="class"]:checked')).map(cb => cb.value),
      ploSelect: Array.from(document.querySelectorAll('input[name="plo"]:checked')).map(cb => cb.value),
      rfwSelect: Array.from(document.querySelectorAll('input[name="rfw"]:checked')).map(cb => cb.value),
      seasonSelect: Array.from(document.querySelectorAll('input[name="season"]:checked')).map(cb => cb.value),
      ppLimit: parseInt(document.getElementById("PPSlide")?.value, 10) || Infinity,
      noappsSelect: Array.from(document.querySelectorAll('input[name="noapps"]:checked')).map(cb => cb.value)
    };

    return data.filter(row => filterDrawPage(row, filterState));
  }

  // ---------- harvest table ----------
  if (harvestTable) {
    return data.filter(filterHarvestPage);
  }

  // ---------- default fallback ----------
  return data;
}

//Laod unit attributes 
let unitAttributes = {};

Papa.parse("DeerHarvest25.csv", {
  download: true,
  header: true,
  skipEmptyLines: true,
  complete: function(results) {
    results.data.forEach(row => {
      const unit = row.harvestunit;
      if (unit) unitAttributes[unit] = row;
    });
  }
});




//Load the draw recap mapping
let huntCodeMap = {};
Papa.parse("deer25code_pages.csv", {
  download: true,
  header: true,
  complete: function(results) {
    results.data.forEach(row => {
      if (row.HuntCode && row.Page) {
        huntCodeMap[row.HuntCode] = row.Page;
      }
    });
  }
});

// Load Data table
function initTable({ tableId, csvFile, columns, headers }) {
  const tableEl = document.getElementById(tableId);
  if (!tableEl) return; // page doesn't have this table

  const renderFn = tableId === "itemTable" ? renderDrawTable : renderHarvestTable;

  let parsedData = [];

  // ---------- Update PP column only when value changes ----------
  let cachedPPValue = null;
  function updateColumnBasedOnPP() {
    const ppInput = document.getElementById("PPSlide");
    if (!ppInput) return;

    const ppValue = parseInt(ppInput.value, 10);
    if (ppValue === cachedPPValue) return; // already up-to-date
    cachedPPValue = ppValue;

    parsedData.forEach(row => {
      const dolValue = parseFloat(row.DOL);
      if (isNaN(dolValue)) return;

      if (dolValue < ppValue) {
        row.Chance_with_First_choice = "100%";
      } else if (Math.round(dolValue) === ppValue) {
        row.Chance_with_First_choice = row.Chance_at_DOL;
      } else {
        row.Chance_with_First_choice = "0%";
      }
    });
  }

  // ---------- Parse CSV ----------
  Papa.parse(csvFile, {
    download: true,
    header: true,
    skipEmptyLines: true,
    complete: function (results) {
      parsedData = results.data;

      // ----- Precompute numeric fields for speed -----
      parsedData.forEach(row => {
        row._validGMUs = String(row["Valid GMUs"] || "")
          .split(",")
          .map(u => parseInt(u.trim(), 10))
          .filter(n => !isNaN(n));

        row._DOL = parseFloat(row.DOL);
        row._ChanceAtDOL = parseFloat(row["Chance_at_DOL"]) || 0;
      });

      // ----- Build table header -----
      const tableHead = tableEl.querySelector("thead");
      tableHead.innerHTML =
        "<tr>" +
        columns.map(h => `<th data-col="${h}">${headers[h] || h} ▲▼</th>`).join("") +
        "</tr>";

      // ---------- Sorting ----------
      tableHead.querySelectorAll("th").forEach(th => {
        th.addEventListener("click", () => {
          toggleSort(th.dataset.col);
          currentPage = 1;
          updateColumnBasedOnPP();
          renderFn(applyFilters(parsedData), currentPage);
        });
      });

      // ---------- Draw Table Filters ----------
      if (tableId === "itemTable") {
        const ppInput = document.getElementById("PPSlide");
        const ppValDisplay = document.getElementById("PPVal");

        if (ppInput) {
          ppInput.addEventListener("input", () => {
            ppValDisplay.textContent = ppInput.value;
            currentPage = 1;
            updateColumnBasedOnPP();
            renderFn(applyFilters(parsedData), currentPage);
          });
        }

        const filters = ["specunit", "sex", "class", "plo", "rfw", "season", "noapps"];
        filters.forEach(id => {
          const el = document.getElementById(id);
          if (el) {
            el.addEventListener("change", () => {
              currentPage = 1;
              renderFn(applyFilters(parsedData), currentPage);
            });
            if (id === "specunit") {
              el.addEventListener("input", () => {
                currentPage = 1;
                renderFn(applyFilters(parsedData), currentPage);
              });
            }
          }
        });

        // --- Start: Resident-only RFW control logic ---
        function updateRFWState() {
          const classVals = Array.from(document.querySelectorAll('input[name="class"]:checked')).map(cb => cb.value);
          // Decide what counts as a resident class — adjust these values to match your CSV/class values if needed
          const residentKeys = ["A_R", "Y_R", "L_R", "L_U"]; // L_U/L_R included if you want landowner to be treated as resident
          const isResident = classVals.some(v => residentKeys.includes(v));

          const rfwContainer = document.getElementById("rfw");
          if (!rfwContainer) return;

          // Find the radio inputs inside the rfw container
          const rfwInputs = Array.from(rfwContainer.querySelectorAll('input[name="rfw"]'));
          const rfwLabels = Array.from(rfwContainer.querySelectorAll("label"));

          if (!isResident) {
            // Not resident -> disable RFW and set to "No" (don't show any RFW tags)
            rfwInputs.forEach(inp => {
              inp.disabled = true;
              // set checked according to preference: choose "N" (No RFW tags) to avoid accidental inclusion
              if (inp.value === "N") inp.checked = true;
              else inp.checked = false;
            });
            // add a visual disabled class so the container looks greyed out
            rfwContainer.classList.add("control-disabled");
          } else {
            // Resident -> enable RFW and default to "Y" (show all tags) if nothing checked
            rfwInputs.forEach(inp => inp.disabled = false);
            rfwContainer.classList.remove("control-disabled");
            // if nothing is currently checked, set default (optional)
            if (!rfwInputs.some(i => i.checked)) {
              const defaultInput = rfwInputs.find(i => i.value === "Y");
              if (defaultInput) defaultInput.checked = true;
            }
          }

          // re-render to apply filter changes
          currentPage = 1;
          // If parsedData is available in this scope, call render; otherwise use the render flow you already call via listeners
          try {
            // prefer the central renderFn via applyFilters: this will keep consistent filtering
            renderDrawTable(applyFilters(parsedData), currentPage);
          } catch (e) {
            // fallback: trigger the main render used in this scope
            renderFn(applyFilters(parsedData), currentPage);
          }
        }

        // Hook updateRFWState to class radio changes (call once here to ensure wiring)
        document.querySelectorAll('input[name="class"]').forEach(cb => {
          cb.addEventListener("change", updateRFWState);
        });

        // Run once to initialize state immediately
        updateRFWState();
        // --- End: Resident-only RFW control logic ---

        // "Any" logic for Sex checkboxes
        document.querySelectorAll('input[name="sex"]').forEach(cb => {
          cb.addEventListener("change", () => {
            const anyBox = document.getElementById("sexAny");
            if (cb.value === "Any" && cb.checked) {
              document.querySelectorAll('input[name="sex"]').forEach(other => {
                if (other.value !== "Any") other.checked = false;
              });
            } else if (cb.checked) {
              anyBox.checked = false;
            }
            currentPage = 1;
            renderFn(applyFilters(parsedData), currentPage);
          });
        });

        // "Any" logic for Season checkboxes
        const seasonContainer = document.getElementById("season");
        if (seasonContainer) {
          seasonContainer.addEventListener("change", e => {
            const target = e.target;
            if (target.name === "season") {
              if (target.value === "Any" && target.checked) {
                seasonContainer.querySelectorAll('input[name="season"]').forEach(cb => {
                  if (cb.value !== "Any") cb.checked = false;
                });
              } else if (target.value !== "Any" && target.checked) {
                const anyEl = document.getElementById("seasonAny");
                if (anyEl) anyEl.checked = false;
              }
              currentPage = 1;
              renderFn(applyFilters(parsedData), currentPage);
            }
          });
        }
      }

      // ---------- Harvest Table Filters ----------
      if (tableId === "deerharvesttable") {
  const harvestCheckboxContainer = document.getElementById("harvestCheckboxes");
  const harvestInputs = ["harvestunit", "minsr", "minpl"];

  if (harvestCheckboxContainer) {
    // get unique categories from CSV
    const categories = [...new Set(parsedData.map(row => row.Category).filter(Boolean))].sort();

    // reference containers
    const harvestBox = document.getElementById("harvestCheckboxes");
    const extraBox = document.getElementById("extraHarvestCheckboxes");

    // categories that should always be visible
    const visibleCats = ["Any","2nd season rifle Antlered (Does not include PLO)", "3rd season rifle Antlered (Does not include PLO)", 
      "4th  season rifle Antlered (Does not include PLO)", "All Archery Seasons", "All Muzzleloader Seasons"];

    // inject "Any" + categories
    ["Any", ...categories].forEach(cat => {
      const label = document.createElement("label");

      const input = document.createElement("input");
      input.type = "checkbox";
      input.value = cat;

      // make "Any" checked by default
      if (cat === "Any") input.checked = true;

      const span = document.createElement("span");
      span.textContent = cat;

      label.appendChild(input);
      label.appendChild(span);

      if (visibleCats.includes(cat)) {
        harvestBox.appendChild(label);
      } else {
        extraBox.appendChild(label);
      }
    });

    // Hide "More…" button if no extras
    if (extraBox.children.length === 0) {
      const toggleBtn = document.getElementById("toggleExtraCats");
      if (toggleBtn) toggleBtn.style.display = "none";
    }

    // Checkbox change logic
    harvestCheckboxContainer.addEventListener("change", e => {
      const target = e.target;
      if (target.tagName === "INPUT" && target.type === "checkbox") {
        if (target.value === "Any" && target.checked) {
          // If "Any" is checked, uncheck everything else
          harvestCheckboxContainer.querySelectorAll("input[type=checkbox]").forEach(cb => {
            if (cb.value !== "Any") cb.checked = false;
          });
          extraBox.querySelectorAll("input[type=checkbox]").forEach(cb => {
            cb.checked = false;
          });
        } else if (target.value !== "Any" && target.checked) {
          // If a real category is checked, uncheck "Any"
          const anyBox = harvestCheckboxContainer.querySelector("input[value=Any]");
          if (anyBox) anyBox.checked = false;
        }
        currentPage = 1;
        renderFn(applyFilters(parsedData), currentPage);
      }
    });

    // Also listen for changes inside extra checkboxes
    extraBox.addEventListener("change", e => {
      const target = e.target;
      if (target.tagName === "INPUT" && target.type === "checkbox") {
        const anyBox = harvestCheckboxContainer.querySelector("input[value=Any]");
        if (anyBox && target.checked) anyBox.checked = false;
        currentPage = 1;
        renderFn(applyFilters(parsedData), currentPage);
      }
    });
  }

  // wire up other harvest inputs
  harvestInputs.forEach(id => {
    const el = document.getElementById(id);
    if (el) {
      const eventType = el.tagName === "INPUT" && el.type === "text" ? "input" : "change";
      el.addEventListener(eventType, () => {
        currentPage = 1;
        renderFn(applyFilters(parsedData), currentPage);
      });
    }
  });

  // Toggle "More" categories
const toggleBtn = document.getElementById("toggleExtraCats");
if (toggleBtn) {
  toggleBtn.addEventListener("click", () => {
    const extraBox = document.getElementById("extraHarvestCheckboxes");
    if (extraBox.style.display === "none") {
      extraBox.style.display = "block";
      toggleBtn.textContent = "Less...";
    } else {
      extraBox.style.display = "none";
      toggleBtn.textContent = "More...";
    }
  });
}

}


      // ---------- Pagination ----------
      const nextBtn = document.getElementById("nextPage");
      if (nextBtn) {
        nextBtn.addEventListener("click", () => {
          const totalPages = Math.max(1, Math.ceil(applyFilters(parsedData).length / rowsPerPage));
          if (currentPage < totalPages) {
            currentPage++;
            renderFn(applyFilters(parsedData), currentPage);
          }
        });
      }

      const prevBtn = document.getElementById("prevPage");
      if (prevBtn) {
        prevBtn.addEventListener("click", () => {
          if (currentPage > 1) {
            currentPage--;
            renderFn(applyFilters(parsedData), currentPage);
          }
        });
      }

      // ---------- Initial render ----------
      updateColumnBasedOnPP();
      renderFn(applyFilters(parsedData), currentPage);
    }
  });
}






// Build unique values for Notes filter
// const notesColumn = parsedData.map(row => row.Notes).filter(v => v && v.trim() !== "");
// const uniqueNotes = [...new Set(notesColumn)].sort();

// // Fill select options
// const notesSelect = document.getElementById("notesSelect");
// uniqueNotes.forEach(value => {
//   const opt = document.createElement("option");
//   opt.value = value;
//   opt.textContent = value;
//   notesSelect.appendChild(opt);
// });

// === Global dropdown toggle helper ===
let harvestExpanded = false;
function toggleCheckboxes() {
  const checkboxes = document.getElementById("harvestcatCheckboxes");
  checkboxes.style.display = harvestExpanded ? "none" : "block";
  harvestExpanded = !harvestExpanded;
}

// Load the right CSV based on the page
document.addEventListener("DOMContentLoaded", () => {
  initTable({
    tableId: "itemTable",
    csvFile: "FullDeer25Final.csv",
    columns: visibleColumnsDrawDeer,
    headers: headerLabelsDrawDeer
  });

  initTable({
    tableId: "deerharvesttable",
    csvFile: "DeerHarvest25.csv",
    columns: visibleColumnsHarvestDeer,
    headers: headerLabelsHarvestDeer,
    extraFilters: ["harvestcat", "harvestunit", "successrate","publicpercent"]
  });

  });
