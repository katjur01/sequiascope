// Registrace layoutů `cola` a `fcose` probíhá v js/index.js
console.log("✅ 1 cytoscape_init.js loaded (multi-patient version)");

// Úložiště pro instance Cytoscape pro různé pacienty
const cytoscapeInstances = {};
const lastHighlightedNodes = {};

// Pomocná funkce pro získání namespace z container ID
function getNamespaceFromId(containerId) {
    // Očekává formát: "network_graph-net_MR1507-cyContainer"
    const match = containerId.match(/app-network_tab_([^-]+)-/);
    return match ? match[1] : null;
}

// Společná funkce pro inicializaci Cytoscape grafu
function initializeCytoscape(containerId, elementsData, isSubset = false) {
    const container = document.getElementById(containerId);
    if (!container) {
        console.error(`Kontejner s ID ${containerId} nebyl nalezen.`);
        return null;
    }

    const patientId = getNamespaceFromId(containerId);
    console.log(`Initializing Cytoscape for patient: ${patientId}, container: ${containerId}`);

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
                    'width': 2,              // ✅ Explicitní šířka
                    'line-color': '#999',    // ✅ Explicitní barva
                    'opacity': 1             // ✅ Explicitní opacity
                }
            }
        ],
        layout: { 
            name: 'cola',
            animate: true,
            fit: true,
            padding: 50
        },
        multiselect: false
    });

    // ✅ Debug po inicializaci
    cytoscapeInstance.ready(function() {
        console.log('🎨 Cytoscape ready:', {
            nodes: cytoscapeInstance.nodes().length,
            edges: cytoscapeInstance.edges().length
        });
        
        // ✅ Zkontroluj edges po layoutu
        const edgesBB = cytoscapeInstance.edges().boundingBox();
        console.log('Edges bounding box after init:', edgesBB);
        
        if (edgesBB.w === 0 && edgesBB.h === 0) {
            console.warn('⚠️ Edges have no positions after init!');
        }
    });

    cytoscapeInstance.on('click', (event) => {
        if (event.target === cytoscapeInstance) {
            console.log("Background deselection is forbidden.");
            cytoscapeInstance.nodes().unselectify();
        } else {
            cytoscapeInstance.nodes().selectify();
        }
    });

    // Sledování výběru (pouze pro hlavní graf)
    if (!isSubset) {
        let selectionTimeout;
        
        cytoscapeInstance.on('select unselect', 'node', function(evt) {
            clearTimeout(selectionTimeout);
            selectionTimeout = setTimeout(() => {
                const selectedNodes = cytoscapeInstance.$('node:selected').map(node => node.data('id'));
                console.log("Vybrané uzly:", selectedNodes);
                
                const ns = getNamespaceForContainer(containerId);
                if (ns) {
                    Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });
                }
            }, 200);
        });
    }

    return cytoscapeInstance;
}

// Pomocná funkce pro získání namespace z containerId
function getNamespaceForContainer(containerId) {
    // Formát: "network_graph-net_MR1507-cyContainer"
    const match = containerId.match(/(app-network_tab_[^-]+)-/);
    return match ? match[1] + '-' : null;
}

// Pomocná funkce pro porovnání polí
function arraysEqual(array1, array2) {
    if (array1.length !== array2.length) return false;
    return array1.every((value, index) => array2.includes(value));
}

// ========== SHINY MESSAGE HANDLERS ==========

// Handler pro inicializaci hlavního grafu
Shiny.addCustomMessageHandler('cy-init', function(data) {
    console.log('🔵 Received cy-init data:', data);
    console.log('📊 Elements received:', {
        nodes: data.elements?.nodes?.length || 0,
        edges: data.elements?.edges?.length || 0
    });
    
    const containerId = data.containerId;
    const patientId = getNamespaceFromId(containerId);
    
    if (!patientId) {
        console.error('Cannot determine patient ID from containerId:', containerId);
        return;
    }

    const instanceKey = `${patientId}_main`;
    
    // ✅ Uložit selection PŘED zničením instance
    const previouslySelectedNodes = cytoscapeInstances[instanceKey] 
        ? cytoscapeInstances[instanceKey].$('node:selected').map(node => node.data('id')) 
        : [];

    // ✅ VŽDY zničit starou instanci (pokud existuje)
    if (cytoscapeInstances[instanceKey]) {
        console.log('🗑️ Destroying old instance');
        cytoscapeInstances[instanceKey].destroy();
        delete cytoscapeInstances[instanceKey];
    }
    
    // ✅ Vytvořit novou instanci
    console.log('✨ Creating fresh Cytoscape instance');
    cytoscapeInstances[instanceKey] = initializeCytoscape(containerId, data.elements);
    
    if (!cytoscapeInstances[instanceKey]) {
        console.error('Failed to create Cytoscape instance!');
        return;
    }
    
    console.log('📊 New instance created:', {
        nodes: cytoscapeInstances[instanceKey].nodes().length,
        edges: cytoscapeInstances[instanceKey].edges().length
    });

    // ✅ Znovu označit uzly po inicializaci
    if (previouslySelectedNodes.length > 0) {
        // Počkat až se layout dokončí
        cytoscapeInstances[instanceKey].one('layoutstop', function() {
            const selector = previouslySelectedNodes.map(id => `#${id}`).join(', ');
            const nodesToSelect = cytoscapeInstances[instanceKey].nodes(selector);
            if (nodesToSelect.length > 0) {
                nodesToSelect.select();
                console.log("✅ Nodes re-selected:", previouslySelectedNodes);
            }
        });
    }
});

// Handler pro podgraf
/*Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('🔵 Received cy-subset data:', data);
    console.log('📊 Elements received:', {
        nodes: data.elements?.nodes?.length || 0,
        edges: data.elements?.edges?.length || 0
    });
    
    const containerId = data.containerId;
    const patientId = getNamespaceFromId(containerId);
    
    if (!patientId) {
        console.error('Cannot determine patient ID from containerId:', containerId);
        return;
    }

    const instanceKey = `${patientId}_subset`;

    if (!cytoscapeInstances[instanceKey]) {
        console.log('✨ Creating new subset instance');
        cytoscapeInstances[instanceKey] = initializeCytoscape(containerId, data.elements, true);
    } else {
        console.log('Updating subset instance');
        
        const cy = cytoscapeInstances[instanceKey];
        
        cy.batch(() => {
            cy.elements().remove();
            
            // OPRAVA: Přidat nodes a edges SAMOSTATNĚ
            if (data.elements.nodes && data.elements.nodes.length > 0) {
                cy.add(data.elements.nodes);
                console.log('Added subset nodes:', data.elements.nodes.length);
            }
            
            if (data.elements.edges && data.elements.edges.length > 0) {
                cy.add(data.elements.edges);
                console.log('Added subset edges:', data.elements.edges.length);
            }
        });

        console.log('After subset update:', {
            nodes: cy.nodes().length,
            edges: cy.edges().length
        });

        cy.style().update();
        cy.resize();
        cy.layout({ name: 'cola', animate: false }).run();
    }
});*/
Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('🟢 Received cy-subset data:', data);
    console.log('📊 Subset elements:', {
        nodes: data.elements?.nodes?.length || 0,
        edges: data.elements?.edges?.length || 0,
        clear: data.clear
    });
    
    const containerId = data.containerId;
    const patientId = getNamespaceFromId(containerId);
    
    if (!patientId) {
        console.error('Cannot determine patient ID from containerId:', containerId);
        return;
    }

    const instanceKey = `${patientId}_subset`;
    
    // ✅ 1. VŽDY nejdřív zničit starou instanci (pokud existuje)
    if (cytoscapeInstances[instanceKey]) {
        console.log('🗑️ Destroying old subset instance');
        try {
            cytoscapeInstances[instanceKey].destroy();
        } catch (e) {
            console.warn('Error destroying subset instance:', e);
        }
        delete cytoscapeInstances[instanceKey];
    }
    
    // ✅ 2. Vyčistit HTML container
    const container = document.getElementById(containerId);
    if (!container) {
        console.error('Subset container not found:', containerId);
        return;
    }
    container.innerHTML = '';
    
    // ✅ 3. Pokud je clear flag nebo prázdná data, skončit zde
    if (data.clear === true || 
        !data.elements || 
        !data.elements.nodes || 
        data.elements.nodes.length === 0) {
        console.log('🧹 Subset cleared, no new instance created');
        return;
    }
    
    // ✅ 4. Vytvořit novou instanci POUZE pokud máme data
    console.log('✨ Creating fresh subset instance');
    cytoscapeInstances[instanceKey] = initializeCytoscape(containerId, data.elements, true);
    
    if (!cytoscapeInstances[instanceKey]) {
        console.error('Failed to create subset instance!');
        return;
    }
    
    console.log('📊 Subset instance created:', {
        nodes: cytoscapeInstances[instanceKey].nodes().length,
        edges: cytoscapeInstances[instanceKey].edges().length
    });
    
    // ✅ 5. Spustit layout až po úplné inicializaci
    setTimeout(() => {
        if (cytoscapeInstances[instanceKey]) {
            cytoscapeInstances[instanceKey].layout({
                name: 'cola',
                animate: true,
                fit: true,
                padding: 30,
                maxSimulationTime: 2000
            }).run();
        }
    }, 50);
});

// Handler pro synchronizaci výběru
/*Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
    const patientId = data.patientId;
    let selectedNodes = data.selected_nodes;
    
    if (typeof selectedNodes === 'string') {
        selectedNodes = [selectedNodes];
    } else if (!Array.isArray(selectedNodes)) {
        console.warn("Expected an array for selected_nodes, received:", selectedNodes);
        return;
    }
    
    console.log("Selected nodes to update for patient", patientId, ":", selectedNodes);
    
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No Cytoscape instance found for patient:", patientId);
        return;
    }
    
    const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));
    
    if (arraysEqual(selectedNodes, currentlySelectedNodes)) {
        console.log("Selection is already up to date. No changes needed.");
        return;
    }
    
    cy.batch(() => {
        cy.nodes(':selected').unselect();
        
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
});*/
Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
    const patientId = data.patientId;
    let selectedNodes = data.selected_nodes;
    
    // Normalizovat vstup
    if (typeof selectedNodes === 'string') {
        selectedNodes = [selectedNodes];
    } else if (!Array.isArray(selectedNodes)) {
        console.warn("Expected an array for selected_nodes, received:", selectedNodes);
        selectedNodes = [];
    }
    
    console.log("🔄 Updating selection for patient", patientId, ":", selectedNodes);
    
    const instanceKey = `${patientId}_main`;
    const cy = cytoscapeInstances[instanceKey];
    
    if (!cy) {
        console.warn("No main Cytoscape instance found for patient:", patientId);
        return;
    }
    
    try {
        const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));
        
        // Pokud je výběr stejný, přeskočit
        if (arraysEqual(selectedNodes, currentlySelectedNodes)) {
            console.log("✓ Selection already up to date");
            return;
        }
        
        cy.batch(() => {
            // Zrušit všechny výběry
            cy.nodes(':selected').unselect();
            
            // Vybrat nové uzly - ale jen ty, co skutečně existují v grafu
            if (selectedNodes.length > 0) {
                const existingNodes = [];
                selectedNodes.forEach(id => {
                    const node = cy.getElementById(id);
                    if (node.length > 0) {
                        existingNodes.push(id);
                        node.select();
                    } else {
                        console.warn(`Node ${id} not found in current graph`);
                    }
                });
                
                if (existingNodes.length > 0) {
                    console.log("✓ Selected nodes:", existingNodes);
                } else {
                    console.warn("⚠️ No matching nodes found for:", selectedNodes);
                }
            }
        });
    } catch (e) {
        console.error('Error updating selection:', e);
    }
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