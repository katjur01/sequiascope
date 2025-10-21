// Registrace layoutů `cola` a `fcose` probíhá v js/index.js
//cytoscape.use(cytoscapePanzoom); 
console.log("✅ cytoscape_init.js loaded");


let cy;
let cySubset;
let lastHighlightedNode = null;

// Společná funkce pro inicializaci Cytoscape grafu
function initializeCytoscape(containerId, elementsData, isSubset = false) {
    const container = document.getElementById(containerId);
    if (!container) {
        console.error(`Kontejner s ID ${containerId} nebyl nalezen.`);
        return null;
    }

    const cytoscapeInstance = cytoscape({
        container: container,
        elements: elementsData,
        style: [
            {
                selector: 'node',
                style: {
                    "text-valign": "center",
                    "text-halign": "center",
                    "font-size": "40",
                  //  "border-color": "purple",
                    "content": "data(label)",
                  //  "border-width": "1px",
                    "width": "mapData(degree, 0, 20, 100, 300)",
                    "height": "mapData(degree, 0, 20, 100, 300)"
                }
            },
            {
                selector: 'node[log2FC<=0]',
                style: {
                    'label': 'data(name)',
                    "background-color": "mapData(log2FC, -10, 0, blue, white)"
                }
            },
            {
                selector: 'node[log2FC>0]',
                style: {
                    'label': 'data(name)',
                    "background-color": "mapData(log2FC, 0, 10, white, red)"
                }
            },
            {
                selector: 'node:selected',
                style: {
                    'label': 'data(name)',
                    'background-color': '#B8E788'
                }
            },
            {
                selector: 'node.highlighted',
                style: {
                    'background-color': 'yellow',
                    'border-color': 'yellow',
                    'border-width': '4px'
                }
            },
            {
                selector: 'node.somVariantBorder',
                style: {
                    // Vnitřní hrana
                    'border-width': 16,                // Šířka vnitřní hrany
                    'border-color': 'green',         // Barva vnitřní hrany
                    'border-style': 'solid',          // Styl vnitřní hrany
                }
            },
            {
                selector: 'node.germVariantBorder',
                style: {
                    // Vnitřní hrana
                    'border-width': 16,                // Šířka vnitřní hrany
                    'border-color': 'purple',         // Barva vnitřní hrany
                    'border-style': 'solid',          // Styl vnitřní hrany
                }
            },
            {
                selector: 'node.fusionBorder',
                style: {
                    // Vnější hrana
                    "outline-width": 12,              // Šířka vnější hrany
                    "outline-color": "orange",        // Barva vnější hrany
                    "outline-offset": 4,              // Vzdálenost od vnitřní hrany
                    "outline-style": "solid",         // Styl vnější hrany
                }
            },
            {
                selector: 'edge',
                style: {
                    'curve-style': 'bezier'
                }
            }
        ],
        layout: { name: 'cola' },  // Výchozí layout
        multiselect: false
    });
    

    cytoscapeInstance.on('click', (event) => {
      if (event.target === cytoscapeInstance) {         // click on the background
        console.log("Background deselection is forbidden.");
        cytoscapeInstance.nodes().unselectify();
      } else {
        cytoscapeInstance.nodes().selectify();
      }
    });

    // Sledování výběru a odznačení uzlů
    if (!isSubset) {

        let selectionTimeout;
        
        cytoscapeInstance.on('select unselect', 'node', function(evt) {
            clearTimeout(selectionTimeout);
            selectionTimeout = setTimeout(() => {
                const selectedNodes = cytoscapeInstance.$('node:selected').map(node => node.data('id'));
                console.log("Vybrané uzly:", selectedNodes);
                
                Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });
            }, 200);
        });

            // Synchronizace vybraných uzlů s hlavním grafem
        Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
            let selectedNodes = data.selected_nodes;
        
            if (typeof selectedNodes === 'string') {
                selectedNodes = [selectedNodes];
            } else if (!Array.isArray(selectedNodes)) {
                console.warn("Expected an array for selected_nodes, received:", selectedNodes);
                return;
            }
        
            console.log("Selected nodes to update:", selectedNodes);
        
            const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));
        
            if (arraysEqual(selectedNodes, currentlySelectedNodes)) {
                console.log("Selection is already up to date. No changes needed.");
                return;
            }
        
            cy.batch(() => {
                // Nejdříve odznačit všechny
                cy.nodes(':selected').unselect();
                
                // Pak označit požadované - použít selector string
                if (selectedNodes.length > 0) {
                    const selector = selectedNodes.map(id => `#${id}`).join(', ');
                    const nodesToSelect = cy.nodes(selector);
                    
                    if (nodesToSelect.length > 0) {
                        nodesToSelect.select();
                        console.log("Nodes selected:", selectedNodes);
                    } else {
                        console.warn("No matching nodes found for:", selectedNodes);
                    }
                }
            });
        });


        function arraysEqual(array1, array2) {
            if (array1.length !== array2.length) return false;
            return array1.every((value, index) => array2.includes(value));
        }

//////////////////////
        // Přidání handleru pro přidání vnitřních hran uzlům s germline variantami

// Řádek ~169, nahradit celou funkci:

Shiny.addCustomMessageHandler('variant-border', function(data) {
    console.log('Received data for variant-border:', data);

    if (!data || !data.type || !Array.isArray(data.nodes)) {
        console.warn("Invalid data provided to variant-border handler.");
        return;
    }

    const typeToClass = {
        'germline': 'germVariantBorder',
        'somatic': 'somVariantBorder',
        'fusion': 'fusionBorder'
    };
    
    const className = typeToClass[data.type];
    
    if (!className) {
        console.warn("Unknown type provided:", data.type);
        return;
    }

    // ✅ NOVÝ KÓD - Batch operace místo jednotlivých removeClass/addClass
    cytoscapeInstance.batch(() => {
        // Odstranit třídu ze všech uzlů najednou
        cytoscapeInstance.nodes(`.${className}`).removeClass(className);
        
        if (data.nodes.length > 0) {
            // ✅ Vytvořit selector string místo iterace
            const selector = data.nodes.map(gene => `[name = "${gene}"]`).join(', ');
            const nodesToStyle = cytoscapeInstance.nodes(selector);
            
            if (nodesToStyle.length > 0) {
                nodesToStyle.addClass(className);
                console.log(`Added ${className} to ${nodesToStyle.length} nodes`);
            } else {
                console.warn("No matching nodes found for genes:", data.nodes);
            }
        }
    });
});

//////////////////////

        Shiny.addCustomMessageHandler('cy-add-node-selection', function(data) {
      
            const selector = `[name = "${data.gene.gene}"]`;
            const nodeToHighlight = cytoscapeInstance.nodes(selector).first();
        
            if (nodeToHighlight.length > 0) {
                if (lastHighlightedNode && lastHighlightedNode.same(nodeToHighlight)) {
                    lastHighlightedNode.removeClass('highlighted');
                    lastHighlightedNode = null;
                    console.log("Node unhighlighted:", nodeToHighlight.data('id'));
                } else {
       
                    cytoscapeInstance.batch(() => {
                        if (lastHighlightedNode) {
                            lastHighlightedNode.removeClass('highlighted');
                        }
                        nodeToHighlight.addClass('highlighted');
                    });
                    
                    console.log("Node highlighted:", nodeToHighlight.data('id'));
                    lastHighlightedNode = nodeToHighlight;
                }
            } else {
                console.error("Node not found for gene:", data.gene);
            }
        });
        
        // Handler pro "Select first neighbors"
        Shiny.addCustomMessageHandler('select-first-neighbors', function(data) {
            // Získání aktuálně vybraných uzlů
            const selectedNodes = cytoscapeInstance.$(':selected');
            if (selectedNodes.length === 0) {
                console.warn("Nebyl vybrán žádný uzel.");
                return;
            }
    
            // Najdeme sousedy (closed neighbourhood)
            const neighbors = selectedNodes.neighborhood().add(selectedNodes); // Přidání původních uzlů
    
            // Označení sousedních uzlů
            neighbors.nodes().select();
    
            // Výpis do konzole pro ladění
            console.log("Vybrané uzly a jejich sousedi (closed neighbourhood):", neighbors.nodes().map(n => n.data('id')));
        });
    }

    return cytoscapeInstance;
}


Shiny.addCustomMessageHandler('fit-selected-nodes', function(data) {
    if (data.nodes && data.nodes.length > 0) {
        // Vycentruje na konkrétní vybrané uzly
        const nodesToFit = cy.nodes().filter(function(node) {
            return data.nodes.includes(node.data('id'));
        });

        if (nodesToFit.length > 0) {
            cy.fit(nodesToFit, 50); // 50 je padding kolem vybraných uzlů
            console.log("Fitting view to selected nodes:", data.nodes);
        } else {
            console.warn("No matching nodes found to fit.");
        }
    } else {
        // Vycentruje na celý graf
        cy.fit(50); // 50 je padding kolem všech uzlů
        console.log("Fitting view to the entire graph.");
    }
});

// Handler for plotting main Network (graph)
Shiny.addCustomMessageHandler('cy-init', function(data) {
    console.log('Received data for main graph:', data);

    const previouslySelectedNodes = cy ? cy.$('node:selected').map(node => node.data('id')) : [];

    if (!cy) {
        cy = initializeCytoscape(cyContainerId, data.elements);
    } else {
        cy.batch(() => {
            cy.elements().remove();
            cy.add(data.elements);
        });

        cy.layout({ name: 'cola', animate: true, fit: true }).run();
    }

    // Znovu označit uzly
    if (previouslySelectedNodes.length > 0) {
        // ✅ Použít selector string místo filter
        const selector = previouslySelectedNodes.map(id => `#${id}`).join(', ');
        cy.nodes(selector).select();
        console.log("Nodes re-selected:", previouslySelectedNodes);
    }
});


// Handler for plotting subNetwork (subgraph)
Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('Received data for subset graph:', data);
    
    if (!cySubset) {
        cySubset = initializeCytoscape(cySubsetContainerId, data.elements, true);
    } else {
      /*
        cySubset.elements().remove();
        cySubset.add(data.elements);
        cySubset.layout({ name: 'cola' }).run();
        */
        cySubset.batch(() => {
            cySubset.elements().remove();   // smaže starý subset
            cySubset.add(data.elements);    // přidá nový
        });

        cySubset.style().update();          // vynutí redraw (canvas-bug)
        cySubset.resize();                  // přepočítá viewport

        cySubset.layout({ name: 'cola', animate: false }).run(); 
    }
});


// Highlight row by clicking 
Shiny.addCustomMessageHandler('highlight-row', function(data) {

    const table = document.querySelector('.Reactable .rt-tbody');
    if (!table) return;
    
    // Odstranit předchozí highlight pomocí CSS třídy místo inline style
    const previousHighlight = table.querySelector('.row-highlighted');
    if (previousHighlight) {
        previousHighlight.classList.remove('row-highlighted');
    }
    
    // Najít řádek s konkrétním genem
    const rows = table.querySelectorAll('.rt-tr-group .rt-tr');
    for (const row of rows) {
        const geneNameCell = row.querySelector('.rt-td:nth-child(2)');
        if (geneNameCell && geneNameCell.textContent.trim() === data.gene.gene) {
            row.classList.add('row-highlighted');
            console.log("Highlighting row for gene:", data.gene.gene);
            break; // Ukončit po nalezení
        }
    }
});


// Handler for layout change
Shiny.addCustomMessageHandler('cy-layout', function(layout) {
    if (cy) {
        cy.layout({ name: layout }).run();
    }
});


