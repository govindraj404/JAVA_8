sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/comptypeitemcatconfiguration/test/integration/pages/MainListReport' ,
        'cmh/comptypeitemcatconfiguration/test/integration/pages/MainObjectPage',
        'cmh/comptypeitemcatconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/comptypeitemcatconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);