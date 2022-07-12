sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/referencetypesconfiguration/test/integration/pages/MainListReport' ,
        'cmh/referencetypesconfiguration/test/integration/pages/MainObjectPage',
        'cmh/referencetypesconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/referencetypesconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);