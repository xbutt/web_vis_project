Shiny.addCustomMessageHandler("plotColors", function(colors) {
    console.log("Received colors from R:", colors);

    const dygraphsElement = document.getElementById("dygraph");
    if (dygraphsElement) {
        const dygraphsWidget = window.HTMLWidgets.getInstance(dygraphsElement);
        if (dygraphsWidget) {
            const dygraphObject = dygraphsWidget.dygraph;
            if (dygraphObject) {
                console.log("Updating Dygraph colors:", colors);
                dygraphObject.updateOptions({ colors: colors });
            } else {
                console.log("Error: Dygraph object not found.");
            }
        } else {
            console.log("Error: Dygraphs widget not found.");
        }
    } else {
        console.log("Error: Dygraph element not found.");
    }
});