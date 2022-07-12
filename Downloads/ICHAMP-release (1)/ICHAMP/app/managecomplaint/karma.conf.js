module.exports = function (config) {
    config.set({
  
      frameworks: ["ui5","qunit"],
      ui5: {
        url: "https://sapui5.hana.ondemand.com",
        mode: "script",
        config: {
          theme: 'sap_belize',
          language: 'EN',
          animation: false,
          compatVersion: 'edge',
          async: true,
          resourceRoots: {
            "cmh.managecomplaint": "./base/webapp"
          }
        },     
        tests: [      
          "cmh/managecomplaint/test/unit/AllTests"
        ]
      },

      client: {			
      qunit: { 
         showUI: true
      }
      },
      browsers: ["ChromeCustomHeadless"],
      singleRun: false,
      autoWatch: true,
      logLevel: config.LOG_INFO,
      customLaunchers: {
        ChromeCustom: {
          base: 'Chrome',
          flags: ['--start-maximized']
        },
        ChromeCustomHeadless: {
          base: 'ChromeHeadless',
          flags: ['--window-size=1600,900','--disable-extensions', '--no-sandbox', '--disable-web-security', '--no-proxy-server']
        }
      },
      
      // level of browser logging
      browserConsoleLogOptions: {
        level: 'warn'
      },
      preprocessors: {
        '**/webapp/!(test|localService)/**/*.js': ['coverage']
      }, 
      coverageReporter: {
              includeAllSources: true,
              reporters: [
                  {
                        type: 'lcovonly',
                        dir: './target/coverage',
                        file: "lcov-opa.info"
                  },
                  {
                        type: 'html',
                        dir: './target/coverage',
                  }
                ]			
        },
        junitReporter: {
              outputDir: "./target/Junit",
              outputFile: "TEST-qunit.xml",
              suite: "",
              useBrowserName: true
        },
        htmlReporter: {
          outputFile: './target/Html/JUnit.html',
                
          // Optional
          pageTitle: 'Test Results',
          subPageTitle: 'Detailed test results for OPA 5',
          groupSuites: true,
          useCompactStyle: true,
          useLegacyStyle: true,
          showOnlyFailed: false
        },
      reporters : [ 'progress', 'coverage', 'junit', 'html' ]
    });
  };