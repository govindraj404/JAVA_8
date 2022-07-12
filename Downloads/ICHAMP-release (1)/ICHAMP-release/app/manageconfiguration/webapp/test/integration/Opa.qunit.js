sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/manageconfiguration/test/integration/pages/MainListReport',
        'cmh/manageconfiguration/test/integration/pages/MainObjectPage',
        'cmh/manageconfiguration/test/integration/OpaJourney'
    ],
    function (JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/manageconfiguration') + '/index.html'
        });


        JourneyRunner.run({
                pages: {
                    onTheMainPage: MainListReport,
                    onTheDetailPage: MainObjectPage
                }
            },
            Journey.run
        );

    }
);