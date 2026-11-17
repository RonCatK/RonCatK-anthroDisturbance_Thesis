# SpaDES Anthropogenic Disturbance Simulation – Module Workflows

This file sketches workflow diagrams for the modules and submodels described in `docs/Appendix 4 - ODD Model Description of the SpaDES Anthropogenic Disturbance Simulation.docx`. Diagrams are written in Mermaid so they can be rendered in VS Code, RMarkdown, Quarto, or converted to images for the thesis.

---

## 1. High‑Level Simulation Event Flow

```mermaid
flowchart TD
    %% Key data objects
    DDT[disturbanceDT(metadata table)]
    SA[studyArea]
    RTM[rasterToMatch]
    DP[disturbanceParameters]
    DL0[sim$disturbances]
    DL[sim$disturbanceList]
    CPL[combined potential layers]
    CDL[currentDisturbanceLayer(per‑year mask)]

    %% Modules
    subgraph M1["anthroDisturbance_DataPrep (Data Preparation)"]
      A1["Submodel A: createDisturbanceList"]
      A2["Submodel B: harmonizeList"]
      A1 --> A2
    end

    subgraph M2["potentialResourcesNT_DataPrep (Potential Resources)"]
      C1["Submodel C: Create Combined Potential Layers"]
    end

    subgraph M3["anthroDisturbance_Generator (Disturbance Generator)"]
      subgraph Init["Initialization at T0"]
        D1["Submodel D: calculateSize"]
        E1["Submodel E: calculateRate"]
        D1 --> E1
      end
      subgraph Loop["Recurring event loop (every runInterval years)"]
        F1["Submodel F: Enlarging Algorithm"]
        G1["Submodel G: Site Selection & Creation"]
        H1["Submodel H: Connectivity Algorithm"]
        I1["Submodel I: replaceListFast (State Update)"]
        F1 --> G1 --> H1 --> I1
      end
    end

    %% DataPrep
    DDT --> A1
    SA --> A1
    RTM --> A1
    A1 --> DL0
    DL0 --> A2
    A2 --> DL

    %% Potential Resources
    DL --> C1
    C1 --> CPL
    CPL --> DL

    %% Generator init and loop
    DL --> D1
    DP --> D1
    D1 --> DP
    E1 --> DP

    DP --> F1
    DL --> F1
    DL --> G1
    DL --> H1
    F1 --> G1
    G1 --> H1
    H1 --> I1
    I1 --> DL
    I1 --> CDL
```

---

## 2. Data Preparation Module – Disturbance Harmonization

```mermaid
flowchart TD
    DDT["disturbanceDT(metadata: dataName, dataClass, URL, etc.)"]
    SA[studyArea]
    RTM[rasterToMatch]

    subgraph DataPrep["anthroDisturbance_DataPrep"]
      A1["Submodel A:createDisturbanceList"]
      A2["Submodel B:harmonizeList"]
      A1 --> A2
    end

    DDT --> A1
    SA --> A1
    RTM --> A1

    A1 -->|"per‑dataset spatial objects(sim$disturbances)"| A2
    A2 -->|"merged & harmonizedsim$disturbanceList"| OUT[Ready for potentialResourcesNT_DataPrep and Generator]
```

---

## 3. Potential Resources Module – Combined Potential Layers

```mermaid
flowchart TD
    DL["sim$disturbanceList(includes potential* entries)"]
    WTC["whatToCombine(combination rules)"]

    subgraph PotRes["potentialResourcesNT_DataPrep"]
      C1["Submodel C: Create Combined Potential Layers"]
      C1_DESC["• createPotentialMining• createPotentialOilGas• re‑scale / rank potential• write back to disturbanceList"]
      C1 --> C1_DESC
    end

    DL --> C1
    WTC --> C1
    C1_DESC -->|"updated potential layers(e.g., potentialMining, potentialOilGas)"| DL2[updated sim$disturbanceList]
```

---

## 4. Disturbance Generator – Initialization (Parameter Calculation)

```mermaid
flowchart TD
    DL[sim$disturbanceList]
    DP0["disturbanceParameters(possibly incomplete)"]
    AUX["External / historical data(ECCC footprints, OLD/NEW rasters)"]

    subgraph Init["anthroDisturbance_Generator – Initialization at T0"]
      D1["Submodel D: calculateSize"]
      E1["Submodel E: calculateRate"]
      D1 --> E1
    end

    DL --> D1
    DP0 --> D1
    D1 -->|"fill disturbanceSize for Generating types"| DP1[updated disturbanceParameters]

    DP1 --> E1
    AUX --> E1
    E1 -->|"fill disturbanceRate forGenerating & Enlarging types"| DP[final disturbanceParameters]
```

---

## 5. Disturbance Generator – Recurring Event Loop

```mermaid
flowchart TD
    DP[disturbanceParameters]
    DL[sim$disturbanceList]
    SA[studyArea]
    RTM[rasterToMatch]
    FIRE["rstCurrentBurn (optional)"]
    AV["featuresToAvoid / DEM (optional)"]

    subgraph Loop["anthroDisturbance_Generator – Every runInterval years"]
      F1["Submodel F:Enlarging Algorithm"]
      G1["Submodel G:Site Selection & Creation (Generating)"]
      H1["Submodel H:Connectivity Algorithm"]
      I1["Submodel I:replaceListFast (State Update)"]
      F1 --> G1 --> H1 --> I1
    end

    DP --> F1
    DP --> G1
    DP --> H1

    DL --> F1
    DL --> G1
    DL --> H1

    SA --> F1
    SA --> G1
    RTM --> F1
    RTM --> G1

    FIRE --> G1
    AV --> H1

    I1 -->|"updated disturbanceList(enlarged, generated, connected)"| DL
    I1 --> CDL["currentDisturbanceLayer(per‑year composite mask)"]

    CDL -->|"feedback to other modules(e.g., habitat, forestry)"| DOWNSTREAM[Downstream SpaDES modules]
```

