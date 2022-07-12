sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/itemcategoriesconfiguration/test/integration/pages/MainListReport' ,
        'cmh/itemcategoriesconfiguration/test/integration/pages/MainObjectPage',
        'cmh/itemcategoriesconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/itemcategoriesconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);