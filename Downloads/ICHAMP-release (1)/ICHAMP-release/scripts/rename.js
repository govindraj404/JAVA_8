require('fs').rename(process.argv[2], process.argv[3], function (err) {
        if (err) 
            console.log(err);
            console.log('File renamed!')
        }
    )
