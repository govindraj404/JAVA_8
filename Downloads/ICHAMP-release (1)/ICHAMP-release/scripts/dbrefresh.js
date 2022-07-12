try {
  const newman = require("newman");
  // call newman.run to pass `options` object and wait for callback
  const basePath = "../devops/";
  const env = process.argv[2] ? process.argv[2] : "dev";
  console.log("Current Environment: " + env);
  newman
    .run({
      collection: require(basePath + "db-refresh.json"),
      environment: require(basePath + "environments/" + env + ".json"),
      reporters: "cli",
    })
    .on("start", function (err, args) {
      // on start of run, log to console
      console.log("DB refresh started...");
    })
    .on("done", function (err, summary) {
      if (err || summary.error) {
        console.error("DB Refresh encountered an error.");
      } else {
        console.log("DB Refresh completed.");
      }
    });
} catch (e) {
  //   if (e) throw e;
  if (e.code == "MODULE_NOT_FOUND") {
    console.log("Install Newman CLI. Run `npm install newman`");
  }
}
