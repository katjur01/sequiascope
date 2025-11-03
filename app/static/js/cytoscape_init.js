// Registrace layoutů `cola` a `fcose` probíhá v js/index.js
console.log("✅ cytoscape_init.js loaded (multi-patient version)");

// Úložiště pro instance Cytoscape pro různé pacienty
const cytoscapeInstances = {};
const lastHighlightedNodes = {};
const updatingFromR = {}; // Flag pro každého pacienta - zabrání zpětné vazbě
const lastSentSelection = {}; // Poslední odeslaná selection pro každého pacienta - zabrání duplicitním updates

// Pomocná funkce pro získání patient ID z container ID
function getPatientIdFromContainerId(containerId) {
    // Očekává formát: "app-network_tab_DZ1601-cyContainer" nebo "app-network_tab_DZ1601-cySubsetContainer"
    const match = containerId.match(/app-network_tab_([^-]+)-/);
    return match ? match[1] : null;
}

// 🔑 DŮLEŽITÉ: Definice stylesheet jako funkce, aby se dala znovu použít
function getCytoscapeStylesheet() {
    return [
        {
            selector: 'node',
            style: {
                "text-valign": "center",
                "text-halign": "center",
                "font-size": "40",
                "content": "data(label)",
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
                'border-width': 16,
                'border-color': 'green',
                'border-style': 'solid',
            }
        },
        {
            selector: 'node.germVariantBorder',
            style: {
                'border-width': 16,
                'border-color': 'purple',
                'border-style': 'solid',
            }
        },
        {
            selector: 'node.fusionBorder',
            style: {
                "outline-width": 12,
                "outline-color": "orange",
                "outline-offset": 4,
                "outline-style": "solid",
            }
        },
        {
            selector: 'edge',
            style: {
                'curve-style': 'bezier',
                'width': 3,
                'line-color': '#999',
                'opacity': 0.7
            }
        }
    ];
}

// Společná funkce pro inicializaci Cytoscape grafu
function initializeCytoscape(containerId, data, isSubset = false) {
    const container = document.getElementById(containerId);
    if (!container) {
        // Tichý return - kontejner ještě nemusí být v DOM (timing při startu)
        return null;
    }
    const elementsData = data.elements
    const patientId = data.patientId
    console.log(`✅ Initializing Cytoscape for patient: ${patientId}, container: ${containerId}, isSubset: ${isSubset}`);

    const cytoscapeInstance = cytoscape({
        container: container,
        elements: elementsData,
        style: getCytoscapeStylesheet(),  // 🔑 Použít funkci pro stylesheet
        layout: { name: 'cola' },
        selectionType: 'additive'  // 🔑 Umožnit výběr více uzlů najednou
    });

    cytoscapeInstance.on('click', (event) => {
        if (event.target === cytoscapeInstance) {
            console.log("Background deselection is forbidden.");
            cytoscapeInstance.nodes().unselectify();
        } else {
            cytoscapeInstance.nodes().selectify();
        }
    });

    // Sledování výběru a odznačení uzlů (pouze pro hlavní graf)
    if (!isSubset) {
        let selectionTimeout;
        // 🔑 Použít globální objekt místo lokální proměnné (kvůli update method)
        const patientId = data.patientId;
        if (!lastSentSelection[patientId]) {
            lastSentSelection[patientId] = [];
        }
        
        cytoscapeInstance.on('select unselect', 'node', function(evt) {
            clearTimeout(selectionTimeout);
            selectionTimeout = setTimeout(() => {
                // 🔑 KLÍČOVÁ OPRAVA: Ignorovat změny když aktualizujeme z R
                if (updatingFromR[patientId]) {
                    console.log("⏭️ Skipping cySelectedNodes update - updating from R");
                    return;
                }
                
                const selectedNodes = cytoscapeInstance.$('node:selected').map(node => node.data('id'));
                
                // 🔑 NOVÁ KONTROLA: Neposílat pokud se selection nezměnila
                if (arraysEqual(selectedNodes, lastSentSelection[patientId])) {
                    console.log("⏭️ Selection unchanged, skipping Shiny update");
                    return;
                }
                
                console.log("Vybrané uzly:", selectedNodes);
                
                // Získat správný namespace pro tento graf
                const ns = getNamespaceForContainer(containerId);
                if (ns) {
                    Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });
                    lastSentSelection[patientId] = selectedNodes.slice(); // Uložit kopii
                }
            }, 200);
        });
    }

    return cytoscapeInstance;
}

// Pomocná funkce pro získání namespace z containerId pro Shiny input
function getNamespaceForContainer(containerId) {
    // Formát: "app-network_tab_MR1507-cyContainer"
    // Vrátit: "app-network_tab_MR1507-" (s trailing dash pro Shiny input)
    const match = containerId.match(/(app-network_tab_[^-]+)-/);
    return match ? match[1] + '-' : null;
}

// Pomocná funkce pro porovnání polí
function arraysEqual(array1, array2) {
    if (array1.length !== array2.length) return false;
    return array1.every((value, index) => array2.includes(value));
}

// ========== SHINY MESSAGE HANDLERS ==========

// Handler pro inicializaci nebo update hlavního grafu
Shiny.addCustomMessageHandler('cy-init', function(data) {
    const startTime = performance.now();
    console.log('⏱️ [cy-init] START: Received cy-init data:', data);
    console.log('⏱️ Data size: Nodes =', data.elements.nodes.length, ', Edges =', data.elements.edges.length);
    
    const containerId = data.containerId;
    const instanceKey = `${data.patientId}_main`;
    
    // Helper function to signal completion
    const signalComplete = (totalTime) => {
        console.log(`⏱️ [cy-init] TOTAL TIME: ${(totalTime / 1000).toFixed(2)}s`);
        const ns = getNamespaceForContainer(containerId);
        if (ns) {
            console.log('📤 Sending cy_render_complete signal to R');
            Shiny.setInputValue(ns + 'cy_render_complete', Math.random(), { priority: "event" });
        }
    };
    
    if (!cytoscapeInstances[instanceKey]) {
        console.log('⏱️ [cy-init] Creating NEW instance...');
        const createStart = performance.now();
        
        cytoscapeInstances[instanceKey] = initializeCytoscape(containerId, data);
        
        const createEnd = performance.now();
        console.log(`⏱️ [cy-init] Instance created in ${((createEnd - createStart) / 1000).toFixed(2)}s`);
        
        const nodes = cytoscapeInstances[instanceKey].nodes().length;
        const edges = cytoscapeInstances[instanceKey].edges().length;
        console.log(`📊 New instance - Nodes: ${nodes}, Edges: ${edges}`);
        
        // 🔑 Signal completion after a short delay (layout is running)
        setTimeout(() => {
            signalComplete(performance.now() - startTime);
        }, 100);
        
    } else {
        console.log('⏱️ [cy-init] UPDATING existing instance...');
        const updateStart = performance.now();
        
        const cy = cytoscapeInstances[instanceKey];
        
        // 🔑 VYČISTIT lastSentSelection - nová pathway, nový stav
        lastSentSelection[data.patientId] = [];
        console.log('🧹 Cleared lastSentSelection cache');
        
        // 🔑 RYCHLÁ AKTUALIZACE: Batch update pro lepší performance
        const batchStart = performance.now();
        cy.batch(() => {
            // Odstranit staré elementy
            cy.elements().remove();
            
            // Přidat nové elementy
            cy.add(data.elements);
            
            // 🔑 KRITICKÉ: IHNED nastavit edge styles (UVNITŘ batch!)
            cy.edges().style({
                'width': 3,
                'line-color': '#999',
                'opacity': 0.7,
                'curve-style': 'bezier'
            });
        });
        const batchEnd = performance.now();
        console.log(`⏱️ [cy-init] Batch update completed in ${((batchEnd - batchStart) / 1000).toFixed(2)}s`);
        
        // 🔑 Spustit layout (bez animace pro rychlost)
        const layoutStart = performance.now();
        const layout = cy.layout({ name: 'cola', animate: false });
        
        layout.on('layoutstop', () => {
            const layoutEnd = performance.now();
            console.log(`⏱️ [cy-init] Layout completed in ${((layoutEnd - layoutStart) / 1000).toFixed(2)}s`);
            
            // 🔑 Signal R that rendering is complete
            signalComplete(performance.now() - startTime);
        });
        
        layout.run();
        
        const nodes = cy.nodes().length;
        const edges = cy.edges().length;
        console.log(`📊 Updated instance - Nodes: ${nodes}, Edges: ${edges}, edge styles applied`);
    }

    // 🔑 NEZNOVU vybírat previouslySelectedNodes - R pošle správnou selection!
    // R pošle update-selected-from-gene-list s KOMPLETNÍ selection včetně uzlů mimo graf
    // (viz R kód: session$sendCustomMessage("update-selected-from-gene-list", ...))
});

// Handler pro podgraf
Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('Received cy-subset data:', data);
    
    const containerId = data.containerId;
    const patientId = data.patientId;
    
    if (!patientId) {
        console.error('Cannot determine patient ID from containerId:', containerId);
        return;
    }

    const instanceKey = `${patientId}_subset`;

    if (!cytoscapeInstances[instanceKey]) {
        cytoscapeInstances[instanceKey] = initializeCytoscape(containerId, data, true);
    } else {
        cytoscapeInstances[instanceKey].batch(() => {
            cytoscapeInstances[instanceKey].elements().remove();
            cytoscapeInstances[instanceKey].add(data.elements);
        });

        cytoscapeInstances[instanceKey].resize();
        // 🔑 NEJDŘÍVE layout
        cytoscapeInstances[instanceKey].layout({ name: 'cola', animate: false }).run();
        // 🔑 PAK stylesheet
        cytoscapeInstances[instanceKey].style(getCytoscapeStylesheet());
    }
});

// Handler pro synchronizaci výběru
Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
    const patientId = data.patientId;
    let selectedNodes = data.selected_nodes;
    
    if (typeof selectedNodes === 'string') {
        selectedNodes = [selectedNodes];
    } else if (!Array.isArray(selectedNodes)) {
        console.warn("Expected an array for selected_nodes, received:", selectedNodes);
        return;
    }
    
    console.log("📥 update-selected-from-gene-list for patient", patientId, ":", selectedNodes);
    
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", patientId);
        return;
    }
    
    const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));
    
    if (arraysEqual(selectedNodes, currentlySelectedNodes)) {
        console.log("✓ Selection already up to date");
        return;
    }
    
    // 🔑 KLÍČOVÁ OPRAVA: Nastavit flag PŘED změnou selection
    updatingFromR[patientId] = true;
    console.log("🔒 Set updatingFromR flag for", patientId);
    
    cy.batch(() => {
        cy.nodes(':selected').unselect();
        
        if (selectedNodes.length > 0) {
            const selector = selectedNodes.map(id => `#${id}`).join(', ');
            const nodesToSelect = cy.nodes(selector);
            
            if (nodesToSelect.length > 0) {
                nodesToSelect.select();
                console.log("✅ Nodes selected:", selectedNodes);
            } else {
                console.warn("⚠️ No matching nodes found for:", selectedNodes);
            }
        }
    });
    
    // 🔑 AKTUALIZOVAT lastSentSelection aby byl synchronizovaný s R
    lastSentSelection[patientId] = selectedNodes.slice();
    
    // 🔑 Resetovat flag po krátké prodlevě (aby se stihly všechny eventy)
    setTimeout(() => {
        updatingFromR[patientId] = false;
        console.log("🔓 Reset updatingFromR flag for", patientId);
    }, 500);
});

// Handler pro variant borders
Shiny.addCustomMessageHandler('variant-border', function(data) {
    console.log('Received data for variant-border:', data);

    if (!data || !data.type || !Array.isArray(data.nodes) || !data.patientId) {
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

    const instanceKey = `${data.patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", data.patientId);
        return;
    }

    cy.batch(() => {
        cy.nodes(`.${className}`).removeClass(className);
        
        if (data.nodes.length > 0) {
            const selector = data.nodes.map(gene => `[name = "${gene}"]`).join(', ');
            const nodesToStyle = cy.nodes(selector);
            
            if (nodesToStyle.length > 0) {
                nodesToStyle.addClass(className);
                console.log(`Added ${className} to ${nodesToStyle.length} nodes`);
            } else {
                console.warn("No matching nodes found for genes:", data.nodes);
            }
        }
    });
});

// Handler pro highlight node
Shiny.addCustomMessageHandler('cy-add-node-selection', function(data) {
    const patientId = data.patientId;
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", patientId);
        return;
    }
    
    const selector = `[name = "${data.gene.gene}"]`;
    const nodeToHighlight = cy.nodes(selector).first();

    if (nodeToHighlight.length > 0) {
        const lastHighlighted = lastHighlightedNodes[patientId];
        
        if (lastHighlighted && lastHighlighted.same(nodeToHighlight)) {
            lastHighlighted.removeClass('highlighted');
            lastHighlightedNodes[patientId] = null;
            console.log("Node unhighlighted:", nodeToHighlight.data('id'));
        } else {
            cy.batch(() => {
                if (lastHighlighted) {
                    lastHighlighted.removeClass('highlighted');
                }
                nodeToHighlight.addClass('highlighted');
            });
            
            console.log("Node highlighted:", nodeToHighlight.data('id'));
            lastHighlightedNodes[patientId] = nodeToHighlight;
        }
    } else {
        console.error("Node not found for gene:", data.gene);
    }
});

// Handler pro select first neighbors
Shiny.addCustomMessageHandler('select-first-neighbors', function(data) {
    const patientId = data.patientId;
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", patientId);
        return;
    }
    
    const selectedNodes = cy.$(':selected');
    if (selectedNodes.length === 0) {
        console.warn("Nebyl vybrán žádný uzel.");
        return;
    }

    const neighbors = selectedNodes.neighborhood().add(selectedNodes);
    neighbors.nodes().select();

    console.log("Vybrané uzly a jejich sousedi:", neighbors.nodes().map(n => n.data('id')));
});

// Handler pro fit selected nodes
Shiny.addCustomMessageHandler('fit-selected-nodes', function(data) {
    const patientId = data.patientId;
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", patientId);
        return;
    }
    
    if (data.nodes && data.nodes.length > 0) {
        const nodesToFit = cy.nodes().filter(function(node) {
            return data.nodes.includes(node.data('id'));
        });

        if (nodesToFit.length > 0) {
            cy.fit(nodesToFit, 50);
            console.log("Fitting view to selected nodes:", data.nodes);
        } else {
            console.warn("No matching nodes found to fit.");
        }
    } else {
        cy.fit(50);
        console.log("Fitting view to the entire graph.");
    }
});

// Handler pro highlight row
Shiny.addCustomMessageHandler('highlight-row', function(data) {
    const table = document.querySelector('.Reactable .rt-tbody');
    if (!table) return;
    
    const previousHighlight = table.querySelector('.row-highlighted');
    if (previousHighlight) {
        previousHighlight.classList.remove('row-highlighted');
    }
    
    const rows = table.querySelectorAll('.rt-tr-group .rt-tr');
    for (const row of rows) {
        const geneNameCell = row.querySelector('.rt-td:nth-child(2)');
        if (geneNameCell && geneNameCell.textContent.trim() === data.gene.gene) {
            row.classList.add('row-highlighted');
            console.log("Highlighting row for gene:", data.gene.gene);
            break;
        }
    }
});

// Handler pro změnu layoutu
Shiny.addCustomMessageHandler('cy-layout', function(data) {
    const patientId = data.patientId;
    const layout = data.layout;
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (cy) {
        cy.layout({ name: layout }).run();
    }
});

// Handler pro skrytí disconnected nodes
Shiny.addCustomMessageHandler('hide-disconnected-nodes', function(data) {
    const patientId = data.patientId;
    const hide = data.hide;
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (cy) {
        if (hide) {
            // Skrýt uzly s degree 0 (žádné hrany)
            cy.nodes().forEach(function(node) {
                if (node.degree() === 0) {
                    node.style('display', 'none');
                }
            });
        } else {
            // Zobrazit všechny uzly
            cy.nodes().style('display', 'element');
        }
    }
});