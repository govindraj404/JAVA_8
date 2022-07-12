sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/complaintchannelconfiguration/test/integration/pages/MainListReport' ,
        'cmh/complaintchannelconfiguration/test/integration/pages/MainObjectPage',
        'cmh/complaintchannelconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/complaintchannelconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);