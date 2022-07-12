// for more information visit https://github.wdf.sap.corp/sProcurement/vyperForAll/blob/master/documentation/topics/configuration.md
exports.config = {

  directConnect: true,

  SELENIUM_PROMISE_MANAGER: false,

  capabilities: {
    "browserName": "chrome",
    "acceptInsecureCerts": true,
    "acceptSslCerts": true,
    chromeOptions: {
      args: [
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--disable-web-security",
        "--disable-infobars",
        "--disable-extensions",
        "--ignore-certificate-errors",
        "--enable-logging",
        "--incognito",
        // "--headless",
        // "--disable-gpu",
        "--window-size=1920,1200",
        // "--start-maximized"       
      ],
      prefs: {
        "profile.password_manager_enabled": false,
        "credentials_enable_service": false,
        "password_manager_enabled": false
      }
    }
  },

  params: {
    clientInterval: 150,
    stepsRetries: 2,
    stepRetriesIntervals: 1000,
    failFast: true,
    dontShowBrowserLogs: false,
    auth: {
      formType: "plain"
    },
    coverage: {
      status: false,
      coverage_files: ["yourComponent"],
      sourcePath: "./sourceFolder"
    },
    import: {
      data: "./data/"
    }
  },

  //baseUrl: "https://tesla-cmh-qa.cfapps.sap.hana.ondemand.com/cp.portal/site#Shell-home",
  // baseUrl: "https://tesla.cpp.cfapps.sap.hana.ondemand.com/site?siteId=ad16a6a2-3de5-44f6-84ff-1a124ca4552d&sap-language=en#Shell-home",
  //baseUrl: "https://ferrari.launchpad.cfapps.eu20.hana.ondemand.com/site?siteId=44cd6bc6-3a6a-46b9-9f61-b1db5f8b43fe#Shell-home",
  baseUrl: "https://ferrari.complaintshandlingqa.cfapps.eu20.hana.ondemand.com/cp.portal/site#complaint-manage?sap-ui-app-id-hint=cmh.managecomplaint",
  
// 
  framework: "jasmine2",

  specs: [
    // "specs/01_ichamp_192.spec.js",
    path.resolve(process.env.VYPER_BO, "businessObjects/ComplaintHandling/ManageComplaints/scenarios/specs/01_ichamp_192.spec.js"),
    //"specs/02_ichamp_195.spec.js",
    path.resolve(process.env.VYPER_BO, "businessObjects/ComplaintHandling/ManageComplaints/scenarios/specs/02_ichamp_195.spec.js"),
    // "specs/07_ichamp_196.spec.js",
    //path.resolve(process.env.VYPER_BO, "businessObjects/ComplaintHandling/ManageComplaints/scenarios/specs/07_ichamp_196.spec.js"),
    // "specs/08_ichamp_197.spec.js",
    // "specs/09_ichamp_203.spec.js",
    // "specs/10_ichamp_210.spec.js",
    // "specs/11_ichamp_800.spec.js",
    // "specs/12_ichamp_207.spec.js",
    // "specs/13_ichamp_962.spec.js",
    // "specs/14_ichamp_208.spec.js",
    // "specs/15_ichamp_209.spec.js",
    // "specs/16_ichamp_204.spec.js",
    // "specs/17_ichamp_873.spec.js",
    // "specs/18_ichamp_940.spec.js",
    // "specs/test.spec.js",
    // "specs/input_sanitization.spec.js",
 /*----------------------------------------------------RoleValidation----------------------------------------------------*/

 path.resolve(process.env.VYPER_BO, "businessObjects/ComplaintHandling/ManageComplaints/scenarios/specs/01_ichamp_192.spec.js"),
 path.resolve(process.env.VYPER_BO, "businessObjects/RPMUserRoleValidation/scenarios/specs/packagingManagerRole.spec.js"),
 path.resolve(process.env.VYPER_BO, "businessObjects/RPMUserRoleValidation/scenarios/specs/partnerRole.spec.js"),
 path.resolve(process.env.VYPER_BO, "businessObjects/RPMUserRoleValidation/scenarios/specs/machineLearningSpecialistRole.spec.js"),

 /*----------------------------------------------------Download Statements----------------------------------------------------*/


  ],

  allScriptsTimeout: 60000,
  getPageTimeout: 60000,
  idleTimeout: 90000,

  jasmineNodeOpts: {
    showColors: true,
    defaultTimeoutInterval: 600000
  }

};
