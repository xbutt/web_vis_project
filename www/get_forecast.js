const locations = [
    { name: "Ashgabat", latitude: 37.9601, longitude: 58.3261 },
    { name: "Bishkek", latitude: 42.8746, longitude: 74.5698 },
    { name: "Dushanbe", latitude: 38.5598, longitude: 68.7864 },
    { name: "Tashkent", latitude: 41.2995, longitude: 69.2401 }
];

const getForecast = function() {
    const fetches = locations.map(location => {
        const url = `https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=${location.latitude}&lon=${location.longitude}`;
        return fetch(url)
            .then(response => {
                if (!response.ok) throw new Error(`HTTP error: ${response.status}`);
                return response.json();
            })
            .then(data => ({
                name: location.name,
                temp: data.properties.timeseries[0].data.instant.details.air_temperature
            }));
    });

    Promise.all(fetches).then(results => {
        const weatherData = {};
        results.forEach(entry => {
            weatherData[entry.name] = entry.temp;  // Assign temperature to city name
        });

        console.log("Sending to Shiny:", weatherData); // Debugging output
        Shiny.setInputValue("weather", weatherData); // Send as an object, not an array
    }).catch(error => console.error("API fetch failed:", error));
};

// Call function when page loads
document.addEventListener("DOMContentLoaded", getForecast);
