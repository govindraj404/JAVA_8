sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/servicematerialconfiguration/test/integration/pages/MainListReport' ,
        'cmh/servicematerialconfiguration/test/integration/pages/MainObjectPage',
        'cmh/servicematerialconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/servicematerialconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);