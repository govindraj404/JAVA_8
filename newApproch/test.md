```typescript

private async uploadPart(

    index: number,

    blob: Blob,

    region: string,

    respFile: SendResponseFile,

    totalDataDone: any,

    time: any,

    fileSize: any,

    file: File,

    orderData: any,

    uploadPartsArray: any,

    countParts: any,

    NUM_CHUNKS: any

  ): Promise<void> {

    try {

      this.partFile = {

        filePartDto: {

          region: region,

          bucket: respFile.bucket,

          uploadId: respFile.uploadId,

          fileObjectKey: respFile.fileObjectKey,

          partNumber: index.toString(),

          contentLength: blob.size,

        },

      };
//Your organization's data cannot be pasted here. Only 1024 characters are allowed.
