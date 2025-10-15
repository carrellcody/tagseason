// const tableHead = document.querySelector("#itemTable thead");
// const tableBody = document.querySelector("#itemTable tbody");
let currentPage = 1;
const rowsPerPage = 100;  
let parsedData = []; // global storage

// Sorting state
let sortColumn = null;
let sortDirection = "asc";
let globalRanges = {}; 
//Laod unit attributes 
let globalMin = null;
let globalMax = null;

// Load unit attributes and compute global numeric ranges for coloring
let deerUnitAttributes = {};
let elkUnitAttributes = {};

//Define columns that are visible for the deer draw table
const visibleColumnsDrawDeer = ["Tag", "Valid GMUs", "Drawn_out_level", "Chance_with_First_choice", "Chance_at_DOL",
  "Sex","Weapon", "Notes"];

//Define columns that are visible for the elk draw table
const visibleColumnsDrawElk = ["Tag", "Valid GMUs", "Drawn_out_level", "Chance_with_First_choice", "Chance_at_DOL",
  "Sex","Weapon", "Notes"];
  
//Define columns that are visible for the subtable in the draw results
const visibleColsSubDrawTable = ["Unit", "Bucks", "Antlerless", "Total Hunters","Percent Success","Category","Acres",
  "Acres Public", "percent_public", "Hunters Density Per Sq. Mile","Hunters Density Per Public Sq. Mile"]; 

//Define columns that are visible for the subtable in the draw results
const visibleColsSubDrawTableelk = ["Unit", "Bulls", "Antlerless", "Total Hunters","Percent Success","Category","Acres",
  "Acres Public", "percent_public", "Hunters Density Per Sq. Mile","Hunters Density Per Public Sq. Mile"]; 

//Define columns that are visible for the deer harvest table
const visibleColumnsHarvestDeer = ["Unit",	"Category",	"Bucks",	"Antlerless",	"Total Harvest",	"Total Hunters",	
  "Percent Success",	"Total Rec. Days", "percent_public","Acres", "Acres Public","Hunters Density Per Sq. Mile","Hunters Density Per Public Sq. Mile"];

  //Define columns that are visible for the elk harvest table
const visibleColumnsHarvestElk = ["Unit",	"Category",	"Bulls",	"Total Antlerless Harvest",	"Total Harvest",	"Total Hunters",	
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

const headerLabelsDrawElk = {
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
  "Acres Public" : "Public Acres",
  "Hunters Density Per Sq. Mile": "Hunters per square mile",
  "Hunters Density Per Public Sq. Mile" : "Hunters per public square mile"
};
const headerLabelsHarvestElk = {
  "Unit": "Unit",
  "Category": "Harvest Category",
  "Bulls":"Bulls Killed",	
  "Total Antlerless Harvest" : "Cows or Calves Killed",
  "Total Harvest" : "Total Harvest",
  "Total Hunters" : "Total Hunters",
  "Percent Success" :"Success Rate",
  "Total Rec. Days" : "Total Rec Days"
  ,"percent_public" : "Public Land %",
  "Acres" : "Total Acres",
  "Acres Public" : "Public Acres",
  "Hunters Density Per Sq. Mile": "Hunters per square mile",
  "Hunters Density Per Public Sq. Mile" : "Hunters per public square mile"
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
const headerLabelsSubDrawelk = {
"Unit":"Unit",		
"Bulls": "Bulls Killed",	
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

//layout fitting below header
function fitLayoutBelowHeader(){
  const header = document.querySelector('.page-header');
  const layout = document.querySelector('.layout');
  if(!layout) return;
  const headerH = header ? header.getBoundingClientRect().height : 0;
  // account for body margins (top + bottom)
  const bodyStyle = getComputedStyle(document.body);
  const mTop = parseFloat(bodyStyle.marginTop) || 0;
  const mBottom = parseFloat(bodyStyle.marginBottom) || 0;
  const available = window.innerHeight - headerH - mTop - mBottom;
  layout.style.height = (available > 200 ? available : 200) + 'px'; // keep a small min height
}
window.addEventListener('load', fitLayoutBelowHeader);
window.addEventListener('resize', fitLayoutBelowHeader);


// Colorize multiple columns, each using its own min/max from globalRanges
function colorizeColumns(table, colHeaderMap) {
  if (!table || !colHeaderMap) return;

  Object.entries(colHeaderMap).forEach(([colIndex, config]) => {
    // support either string (old way) or object with reverse
    let headerName, reverse;
    if (typeof config === "string") {
      headerName = config;
      reverse = false; // default old behavior
    } else {
      headerName = config.name;
      reverse = config.reverse ?? false;
    }

    const range = globalRanges[headerName];
    if (!range) return;

    const { min, max } = range;

    Array.from(table.querySelectorAll("tbody tr")).forEach((row, rowIndex) => {
      if (rowIndex === 0) return; // skip header row
      const cell = row.cells[colIndex];
      if (!cell) return;

      const raw = String(cell.textContent ?? "").trim();
      const cleaned = raw.replace(/[,()%\s]/g, "").replace(/[^\d.\-]/g, "");
      const value = parseFloat(cleaned);
      if (isNaN(value)) return;

      let ratio = (value - min) / (max - min);
      ratio = Math.max(0, Math.min(1, ratio));

      if (reverse) ratio = 1 - ratio;

      const hue = ratio * 120; // red → green
      const saturation = 50;
      const lightness = 75;

      cell.style.backgroundColor = `hsl(${hue}, ${saturation}%, ${lightness}%)`;
    });
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

  // add click handler for expanding/collapsing sub-table
  tr.addEventListener("click", () => {
    const nextRow = tr.nextSibling;
    const firstTd = tr.querySelector("td span"); // the arrow span
    console.log("▶️ Expanding draw row for:", row.harvestunit);

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
      const isElk = tableEl.id === "elkitemtable";
      const harvestMap = isElk ? elkUnitAttributes : deerUnitAttributes;
      const visibleCols = isElk ? visibleColsSubDrawTableelk : visibleColsSubDrawTable;
      const headerLabels = isElk ? headerLabelsSubDrawelk : headerLabelsSubDraw;

      visibleCols.forEach(vc => {
        const th = document.createElement("th");
        th.textContent = headerLabels[vc] || vc;
        headerRow.appendChild(th);
      });
      subTable.appendChild(headerRow);

// extract GMUs from the main draw table row
const harvestUnits = String(row.harvestunit || "")
  .split(",")
  .map(u => u.trim())
  .filter(u => u !== "");



harvestUnits.forEach(unitKey => {
  const harvestRow = harvestMap[unitKey];
  if (harvestRow) {
    const dataRow = document.createElement("tr");

    const visibleCols = isElk ? visibleColsSubDrawTableelk : visibleColsSubDrawTable;

    visibleCols.forEach(uc => {
      const td = document.createElement("td");
      if (uc === "Unit") {
        const unitName = harvestMap[unitKey][uc] ?? "";
        const url = harvestMap[unitKey]["onx"];
        if (unitName && url) {
          td.innerHTML = `<a href="${url}" target="_blank">${unitName}</a>`;
        } else {
          td.textContent = unitName;
        }
      } else {
        td.textContent = harvestMap[unitKey][uc] ?? "";
      }
      dataRow.appendChild(td);
    });

    subTable.appendChild(dataRow);
  }
});



        subTd.appendChild(subTable);
        subTr.appendChild(subTd);
        tr.parentNode.insertBefore(subTr, tr.nextSibling);

        // colorize just this subtable
        const colMap = {
          8: "percent_public",
          9: "Hunters Density Per Sq. Mile",
         10: "Hunters Density Per Public Sq. Mile",
          };



    Object.entries(colMap).forEach(([colIndex, headerName]) => {
  colorizeColumns(subTable, {
  9: {name: "Hunters Density Per Sq. Mile", reverse: true},
  10: {name: "Hunters Density Per Public Sq. Mile", reverse: true},
  8: {name: "percent_public", reverse: false}
    });

});

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

    // ✅ If this is the Unit column, make it a clickable link
    if (col === "Unit") {
      const unit = row[col];
      const url = row["onx"] || row["Map_URL"] || ""; // adjust to your CSV column name
      if (unit && url) {
        const a = document.createElement("a");
        a.href = url;
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        a.textContent = unit;
        td.appendChild(a);
      } else {
        td.textContent = unit ?? "";
      }
    } else {
      td.textContent = row[col] ?? "";
    }

    tr.appendChild(td);
  });
  body.appendChild(tr);
});

}

// --- Render ELK Draw Table ---
function renderElkDrawTable(
  data,
  page = 1,
  columns = visibleColumnsDrawElk,
  tableEl = document.getElementById("elkitemtable")
) {
  renderDrawTable(data, page, columns, tableEl);
}

// --- Render ELK Harvest Table ---
function renderElkHarvestTable(data, page = 1) {
  const body = document.querySelector("#elkharvesttable tbody");
  body.innerHTML = "";

  const sorted = sortData(data);
  const rows = sorted.slice((page-1)*rowsPerPage, page*rowsPerPage);

  rows.forEach(row => {
    const tr = document.createElement("tr");
    visibleColumnsHarvestElk.forEach(col => {
      const td = document.createElement("td");

      if (col === "Unit") {
        const unit = row[col];
        const url = row["onx"] || row["Map_URL"] || "";
        if (unit && url) {
          const a = document.createElement("a");
          a.href = url;
          a.target = "_blank";
          a.rel = "noopener noreferrer";
          a.textContent = unit;
          td.appendChild(a);
        } else {
          td.textContent = unit ?? "";
        }
      } else {
        td.textContent = row[col] ?? "";
      }

      tr.appendChild(td);
    });
    body.appendChild(tr);
  });
}



// Apply filters
function applyFilters(data) {
  const deerDrawTable = document.getElementById("itemTable");
  const elkDrawTable = document.getElementById("elkitemtable");
  const harvestTable = document.getElementById("deerharvesttable");
  const elkHarvestTable = document.getElementById("elkharvesttable");

  // ---------- filters for any Draw page (deer, elk, etc.) ----------
  function filterDrawPage(row, filterState) {
    let match = true;

    const {
      nameSearch,
      sexSelect,
      classSelect,
      ploSelect,
      rfwSelect,
      seasonSelect,
      ppLimit,
      ppmin,
      noappsSelect
    } = filterState;

    // ---- Specific Unit search ----
    if (nameSearch) {
      const searchUnits = nameSearch
        .split(",")
        .map(u => parseInt(u.trim(), 10))
        .filter(n => !isNaN(n));
      const rowUnits = String(row["Valid GMUs"] || "")
        .split(",")
        .map(u => parseInt(u.trim(), 10))
        .filter(n => !isNaN(n));
      if (!searchUnits.some(su => rowUnits.includes(su))) match = false;
    }

    // Class filter
    if (classSelect.length && !classSelect.includes(row.Class)) match = false;

    // Sex filter with "Any" logic
    if (sexSelect.length && !sexSelect.includes("Any") && !sexSelect.includes(row.Sex)) match = false;

    // Season filter
    if (seasonSelect.length && !seasonSelect.includes("Any")) {
      const definedSeasons = ["A","M","O1R","O2R","O3R","O4R","E","L","U"];
      const SeasonWeapon = (row.SeasonWeapon || "Any").trim().toUpperCase();
      if (seasonSelect.includes("Other")) {
        if (definedSeasons.some(prefix => SeasonWeapon.includes(prefix))) match = false;
      } else if (!seasonSelect.some(sel => SeasonWeapon.includes(sel))) {
        match = false;
      }
    }

    // ✅ PP (DOL) filter
    const dolValue = parseFloat(row.DOL);
    if (!isNaN(dolValue)) {
      if (!isNaN(ppmin) && dolValue < ppmin) match = false;
      if (!isNaN(ppLimit) && dolValue > ppLimit) match = false;
    }

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
    const harvestCats = Array.from(
      document.querySelectorAll("#harvestCheckboxContainer input[type=checkbox]:checked")
    ).map(cb => cb.value);

    const harvestUnit = document.getElementById("harvestunit")?.value.trim();
    const minSuccess = parseFloat(document.getElementById("minsr")?.value);
    const minPL = parseFloat(document.getElementById("minpl")?.value);

    let match = true;

    // Category filter
    if (harvestCats.length && !harvestCats.includes("Any")) {
      if (!harvestCats.includes(row.Category)) match = false;
    }

    // Unit filter
    if (harvestUnit) {
      const units = harvestUnit.split(",").map(u => u.trim()).filter(u => u !== "");
      if (!units.includes(String(row.Unit))) match = false;
    }

    // Success rate filter
    if (minSuccess) {
      const rowRate = parseFloat(row["Percent Success"]);
      if (isNaN(rowRate) || rowRate < minSuccess) match = false;
    }

    // Percent Public Land filter
    if (minPL) {
      const rowpublic = parseFloat(row["percent_public"]);
      if (isNaN(rowpublic) || rowpublic < minPL) match = false;
    }

    return match;
  }

  // ---------- gather filter input state once for draw tables ----------
  if (deerDrawTable || elkDrawTable) {
    const filterState = {
      nameSearch: document.getElementById("specunit")?.value.trim() || "",
      sexSelect: Array.from(document.querySelectorAll('input[name="sex"]:checked')).map(cb => cb.value),
      classSelect: Array.from(document.querySelectorAll('input[name="class"]:checked')).map(cb => cb.value),
      ploSelect: Array.from(document.querySelectorAll('input[name="plo"]:checked')).map(cb => cb.value),
      rfwSelect: Array.from(document.querySelectorAll('input[name="rfw"]:checked')).map(cb => cb.value),
      seasonSelect: Array.from(document.querySelectorAll('input[name="season"]:checked')).map(cb => cb.value),
      ppLimit: parseInt(document.getElementById("PPSlide")?.value, 10),
      ppmin: parseInt(document.getElementById("ppmin")?.value, 10),
      noappsSelect: Array.from(document.querySelectorAll('input[name="noapps"]:checked')).map(cb => cb.value)
    };

    return data.filter(row => filterDrawPage(row, filterState));
  }

  // ---------- harvest table ----------
  if (harvestTable || elkHarvestTable) {
    return data.filter(filterHarvestPage);
  }

  // ---------- fallback ----------
  return data;
}





Papa.parse("DeerHarvest25.csv", { 
  download: true,
  header: true,
  dynamicTyping: false,
  complete: function(results) {
    results.data.forEach(row => {
      const unit = String(row.harvestunit || "").trim();
      if (unit) deerUnitAttributes[unit] = row; // use alphanumeric key
    });

    // 🔧 Percentile cutoff (e.g., 0.05 = 5%)
    const PERCENTILE_CLIP = 0.1;  

    // Which CSV headers should we compute ranges for?
    const targetCols = [
      "Hunters Density Per Sq. Mile",
      "Hunters Density Per Public Sq. Mile",
      "percent_public"
      // add more column header names here if you want them colored
    ];

    targetCols.forEach(col => {
      // extract numeric values, cleaning commas, percent signs, whitespace, etc.
      const values = results.data
        .map(r => {
          const raw = String(r[col] ?? "").trim();
          const cleaned = raw.replace(/[,()%\s]/g, "").replace(/[^\d.\-]/g, "");
          const n = parseFloat(cleaned);
          return isNaN(n) ? null : n;
        })
        .filter(v => v !== null)
        .sort((a, b) => a - b); // sort ascending for percentiles

      if (values.length === 0) {
        console.warn(`colorize: no numeric values found for column "${col}"`);
        return;
      }

      const n = values.length;
      const lowIndex = Math.floor(n * PERCENTILE_CLIP);
      const highIndex = Math.floor(n * (1 - PERCENTILE_CLIP));

      const minClipped = values[lowIndex];
      const maxClipped = values[highIndex];

      globalRanges[col] = {
        min: minClipped,
        max: maxClipped,
        values: values // store all numeric values for percentile scaling
      };

      console.info(`colorize: globalRanges[${col}] =`, globalRanges[col]);
    });
  },
  error: function(err) {
    console.error("Error parsing DeerHarvest25.csv for color ranges:", err);
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

    let renderFn;
    switch (tableId) {
      case "itemTable":
        renderFn = renderDrawTable;
        break;
      case "elkitemtable":
        renderFn = renderElkDrawTable;
        break;
      case "deerharvesttable":
        renderFn = renderHarvestTable;
        break;
      case "elkharvesttable":
        renderFn = renderElkHarvestTable;
        break;
      default:
        return; // Unknown table
    }

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
      if (tableId === "itemTable" || tableId === "elkitemtable") {
        const ppInput = document.getElementById("PPSlide");
        const ppValDisplay = document.getElementById("PPSlide");
        const ppmin = document.getElementById("ppmin");

        if (ppInput) {
          ppInput.addEventListener("input", () => {
            ppValDisplay.textContent = ppInput.value;
            currentPage = 1;
            updateColumnBasedOnPP();
            renderFn(applyFilters(parsedData), currentPage);
          });
        }

        if (ppmin) {
          ppmin.addEventListener("input", () => {
            currentPage = 1;
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
          const residentKeys = ["A_R", "Y_R", "L_R", "L_U"];
          const isResident = classVals.some(v => residentKeys.includes(v));

          const rfwContainer = document.getElementById("rfw");
          if (!rfwContainer) return;

          const rfwInputs = Array.from(rfwContainer.querySelectorAll('input[name="rfw"]'));
          const rfwLabels = Array.from(rfwContainer.querySelectorAll("label"));

          if (!isResident) {
            rfwInputs.forEach(inp => {
              inp.disabled = true;
              if (inp.value === "N") inp.checked = true;
              else inp.checked = false;
            });
            rfwContainer.classList.add("control-disabled");
          } else {
            rfwInputs.forEach(inp => inp.disabled = false);
            rfwContainer.classList.remove("control-disabled");
            if (!rfwInputs.some(i => i.checked)) {
              const defaultInput = rfwInputs.find(i => i.value === "Y");
              if (defaultInput) defaultInput.checked = true;
            }
          }

          renderFn(applyFilters(parsedData), currentPage);
        }

        document.querySelectorAll('input[name="class"]').forEach(inp => {
          inp.addEventListener("change", updateRFWState);
        });

        // initialize the RFW state once at start
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
      if (tableId === "deerharvesttable"|| tableId == "elkharvesttable") {
  const harvestCheckboxContainer = document.getElementById("harvestCheckboxes");
  const harvestInputs = ["harvestunit", "minsr", "minpl"];

  if (harvestCheckboxContainer) {
  // get unique categories from CSV
  const categories = [...new Set(parsedData.map(row => row.Category).filter(Boolean))];

  // reference containers
  const harvestBox = document.getElementById("harvestCheckboxes");
  const extraBox = document.getElementById("extraHarvestCheckboxes");
  
  let visibleCats = [];
  let customOrder = [];

  // categories that should always be visible
  if (tableId === "deerharvesttable") {
  visibleCats = [
    "All manners of take",
    "2nd season rifle Antlered (Does not include PLO)",
    "3rd season rifle Antlered (Does not include PLO)",
    "4th  season rifle Antlered (Does not include PLO)",
    "All Archery Seasons",
    "Antlered Muzzleloader",
    "Antlerless Muzzleloader"
  ];

  // custom order comes first
  customOrder = [
    "All manners of take",
    "2nd season rifle Antlered (Does not include PLO)",
    "3rd season rifle Antlered (Does not include PLO)",
    "4th  season rifle Antlered (Does not include PLO)",
    "Antlered Muzzleloader",
    "Antlerless Muzzleloader",
    "All Archery Seasons"
  ];
  }

  if (tableId === "elkharvesttable") {
  visibleCats = [
    "All manners of take",
    "Antlered Second Rifle Seasons (No PLO or Either-sex tags included)",
    "Antlered Third Rifle Seasons (No PLO or Either-sex tags included)",
    "Antlered Fourth Rifle Seasons (No PLO or Either-sex tags included)",
    "All Archery Seasons",
    "Antlered Muzzleloader",
    "Antlerless Muzzleloader"
  ];
  
  // custom order comes first
  customOrder = [
    "All manners of take",
    "Antlered Second Rifle Seasons (No PLO or Either-sex tags included)",
    "Antlered Third Rifle Seasons (No PLO or Either-sex tags included)",
    "Antlered Fourth Rifle Seasons (No PLO or Either-sex tags included)",
    "All Archery Seasons",
    "Antlered Muzzleloader",
    "Antlerless Muzzleloader"
  ];
  }
  // categories not in custom order
  const remainingCats = categories.filter(c => !customOrder.includes(c)).sort();

  // final ordered list (without "Any")
  const orderedCats = [...customOrder, ...remainingCats];

  // inject main categories
  orderedCats.forEach(cat => {
    const label = document.createElement("label");

    const input = document.createElement("input");
    input.type = "checkbox";
    input.value = cat;

    // make "All manners of take" checked by default
    if (cat === "All manners of take") input.checked = true;

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

  // now add "Any" at the very bottom of the extraBox
  const anyLabel = document.createElement("label");
  const anyInput = document.createElement("input");
  anyInput.type = "checkbox";
  anyInput.value = "Any";

  const anySpan = document.createElement("span");
  anySpan.textContent = "Any";

  anyLabel.appendChild(anyInput);
  anyLabel.appendChild(anySpan);
  extraBox.appendChild(anyLabel);

  // Hide "More…" button if no extras (besides Any)
  if (extraBox.children.length === 1) {
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
          if (cb.value !== "Any") cb.checked = false;
        });
      } else if (target.value !== "Any" && target.checked) {
        // If a real category is checked, uncheck "Any"
        const anyBox = extraBox.querySelector("input[value=Any]");
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
      if (target.value !== "Any" && target.checked) {
        const anyBox = extraBox.querySelector("input[value=Any]");
        if (anyBox) anyBox.checked = false;
      }
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
      extraBox.style.display = "grid";
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

// ------------------------------
// Load ELK Harvest data for unit attributes & color ranges
// ------------------------------
Papa.parse("elkHarvest25.csv", {
  download: true,
  header: true,
  dynamicTyping: false,
  complete: function(results) {
    results.data.forEach(row => {
      const unit = String(row.harvestunit || "").trim();
      if (unit) elkUnitAttributes[unit] = row; // identical logic
    });
    
    const PERCENTILE_CLIP = 0.1;
    const targetCols = [
      "Hunters Density Per Sq. Mile",
      "Hunters Density Per Public Sq. Mile",
      "percent_public"
    ];

    targetCols.forEach(col => {
      const values = results.data
        .map(r => {
          const raw = String(r[col] ?? "").trim();
          const cleaned = raw.replace(/[,()%\s]/g, "").replace(/[^\d.\-]/g, "");
          const n = parseFloat(cleaned);
          return isNaN(n) ? null : n;
        })
        .filter(v => v !== null)
        .sort((a, b) => a - b);

      if (values.length === 0) return;

      const n = values.length;
      const lowIndex = Math.floor(n * PERCENTILE_CLIP);
      const highIndex = Math.floor(n * (1 - PERCENTILE_CLIP));

      globalRanges[col] = {
        min: values[lowIndex],
        max: values[highIndex],
        values
      };
    });
  },
  error: function(err) {
    console.error("Error parsing elkHarvest25.csv:", err);
  }
});

// ------------------------------
// Load ELK HuntCode → PDF Page Map
// ------------------------------
let elkHuntCodeMap = {};
Papa.parse("elk25code_pages.csv", {
  download: true,
  header: true,
  complete: function(results) {
    results.data.forEach(row => {
      if (row.HuntCode && row.Page) {
        elkHuntCodeMap[row.HuntCode] = row.Page;
      }
    });
  }
});




// === Global dropdown toggle helper ===
let harvestExpanded = false;
function toggleCheckboxes() {
  const checkboxes = document.getElementById("harvestcatCheckboxes");
  checkboxes.style.display = harvestExpanded ? "none" : "block";
  harvestExpanded = !harvestExpanded;
}

// Load the right CSV based on the page
document.addEventListener("DOMContentLoaded", () => {
  
  if (document.body.classList.contains("deerdraw")) {
    initTable({
      tableId: "itemTable",
      csvFile: "FullDeer25Final.csv",
      columns: visibleColumnsDrawDeer,
      headers: headerLabelsDrawDeer
    });
  }

  if (document.body.classList.contains("deerharvest")) {
    initTable({
      tableId: "deerharvesttable",
      csvFile: "DeerHarvest25.csv",
      columns: visibleColumnsHarvestDeer,
      headers: headerLabelsHarvestDeer
    });
  }

  if (document.body.classList.contains("elkdraw")) {
    initTable({
      tableId: "elkitemtable",
      csvFile: "Fullelk25Final.csv",
      columns: visibleColumnsDrawElk,
      headers: headerLabelsDrawElk
    });
  }

  if (document.body.classList.contains("elkharvest")) {
    initTable({
      tableId: "elkharvesttable",
      csvFile: "elkHarvest25.csv",
      columns: visibleColumnsHarvestElk,
      headers: headerLabelsHarvestElk
    });
  }
});
