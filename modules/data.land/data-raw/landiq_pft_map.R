non_crop <- c(
    "Flowers, Nursery and Christmas Tree Farms", "Greenhouse",
    "Idle", "Managed Wetland", "Urban"
)

woody_crops <- c(
    "Almonds", "Apples", "Avocados", "Bush Berries", "Cherries",
    "Citrus", "Dates", "Grapes", "Kiwis", "Miscellaneous Deciduous",
    "Miscellaneous Subtropical Fruits", "Olives", "Peaches/Nectarines",
    "Pears", "Pistachios", "Plums, Prunes and Apricots", "Pomegranates",
    "Walnuts", "Young Perennials"
)   

annual_crops <- c(
    "Alfalfa and Alfalfa Mixtures", "Beans (Dry)", "Carrots", "Cole Crops",
    "Corn, Sorghum and Sudan", "Cotton", "Lettuce/Leafy Greens",
    "Melons, Squash and Cucumbers", "Miscellaneous Field Crops",
    "Miscellaneous Grain and Hay", "Miscellaneous Grasses", "Miscellaneous Truck Crops",
    "Mixed Pasture", "Onions and Garlic", "Peppers", "Potatoes and Sweet Potatoes",
    "Rice", "Safflower", "Strawberries", "Sunflowers", "Tomatoes",
    "Wheat", "Wild Rice"
)

landiq_pft_map <- dplyr::bind_rows(
    dplyr::tibble(
        crop = woody_crops,
        pft = "woody perennial crop"
    ),
    dplyr::tibble(
        crop = non_crop,
        pft = "non-crop"
    ),
    dplyr::tibble(
        crop = annual_crops,
        pft = "annual crop"
    )
)
