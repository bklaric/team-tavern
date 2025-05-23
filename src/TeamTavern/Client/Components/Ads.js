export function createAd(placementName) {
    return function (element) {
        return function (writePlacement) {
            return function () {
                self.__VM.push(function (admanager, scope) {
                    console.log("createAd", placementName)
                    if (placementName === "vertical_sticky") {
                        scope.Config.verticalSticky().display();
                    } else if (
                        placementName === "horizontal_sticky" ||
                        placementName === "mobile_horizontal_sticky" ||
                        placementName === "video_slider"
                    ) {
                        var placement = scope.Config.get(placementName).displayBody();
                        writePlacement(placement)()
                    } else {
                        var placement = scope.Config.get(placementName).display(element);
                        writePlacement(placement)()
                    }
                });
            }
        }
    }
}

export function removeAd(placementName) {
    return function (placement) {
        return function () {
            self.__VM.push(function (admanager, scope) {
                console.log("removeAd", placementName)
                if (placementName === "vertical_sticky") {
                    scope.Config.verticalSticky().destroy();
                } else {
                    placement.remove();
                }
            });
        }
    }
}

export function refreshAd(placementName) {
    return function (placement) {
        return function (writePlacement) {
            return function () {
                console.log("refreshAd", placementName, placement);
                self.__VM.push(function (admanager, scope) {
                    if (placementName === "vertical_sticky") {
                        scope.Config.verticalSticky().reload();
                    }
                    else if (
                        placementName === "horizontal_sticky" ||
                        placementName === "mobile_horizontal_sticky" ||
                        placementName === "video_slider"
                    ) {
                        if (placement.activeSize() == null) {
                            // If the horizontal sticky has been closed, we need to create it again.
                            console.log("creating new horizontal sticky")
                            var newPlacement = scope.Config.get(placementName).displayBody();
                            writePlacement(newPlacement)()
                        }
                        else {
                            // Otherwise, just reload the existing placement.
                            placement.reload();
                        }
                    }
                    else {
                        placement.reload();
                    }
                });
            }
        }
    }
}
