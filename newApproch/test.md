learning github
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
      //Generate presigned URL for each part
      const uploadUrlPresigned = await this.preSignedPartUrl(this.partFile);
      //Put each file part into server
      if (uploadUrlPresigned) {
        orderData.push({
          presignedUrl: uploadUrlPresigned.filePartDto.presignedUrl,
          index: index,
        });
        const req = new HttpRequest(
          'PUT',
          uploadUrlPresigned.filePartDto.presignedUrl,
          blob,
          {
            reportProgress: true,
          }
        );
        await new Promise<void>((resolve, reject) => {
          let retryCount = 0;
          // HttpClient to make the request
          const sendRequest = () => {
            this.httpClient.request(req).subscribe({
              next: (event: HttpEvent<any>) => {
                switch (event.type) {
                  case HttpEventType.UploadProgress:
                    //Calculate upload progress and emit progress event
                    let percentDone = 0;
                    totalDataDone[index - 1] = {
                      part: index,
                      loaded: event.loaded,
                    };
                    totalDataDone.forEach(
                      (a: { loaded: number }) => (percentDone += a.loaded)
                    );
                    const diff = new Date().getTime() - time;
                    this.uploadProgress$.next({
                      progress: Math.round((percentDone / fileSize) * 100),
                      token: respFile.tempStrId,
                      currentPart: index,
                      totalSize: fileSize,
                      fileName: file.name,
                      speed: Math.round((percentDone / diff) * 1000),
                    });
                    break;
                  case HttpEventType.Response:
                    if (event instanceof HttpResponse) {
                      const currentPresigned = orderData.find(
                        (item: any) => item.presignedUrl === event.url
                      );
                      countParts++;
                      uploadPartsArray.push({
                        etag: event.headers
                          .get('ETag')
                          ?.replace(/[|&;$%@"<>()+,]/g, ''),
                        partNumber: currentPresigned?.index,
                      });
                      //Check if all parts have been uploaded
                      if (uploadPartsArray.length === NUM_CHUNKS) {
                        //Complete the multiPart Upload
                        lastValueFrom(
                          this.httpClient.post(
                            `${this.url}/completeMultipart`,
                            {
                              file: {
                                region: region,
                                bucket: respFile.bucket,
                                fileId: respFile.fileId,
                                fileObjectKey: respFile.fileObjectKey,
                                completedPartList: uploadPartsArray.sort(
                                  (a: any, b: any) => {
                                    return a.partNumber - b.partNumber;
                                  }
                                ),
                                uploadId: respFile.uploadId,
                              },
                            }
                          )
                        ).then(() => {
                          // Add successfully uploaded file to the successList
                          this.successList.push(file);
                          //check if all files have been successfully uploaded
                          if (
                            this.successList.length === this.uploadFileCount
                          ) {
                            this.uploadCompleted$.emit(this.successList);
                          }
                        });
                      }
                    }
                }
              },
              error: (error) => {
                if (retryCount < this.maxPartRetriesCount) {
                  retryCount++;
                  setTimeout(() => sendRequest(), 1000);
                } else {
                  console.log('Max retries reached. Give up.');
                  reject(error);
                  this.stopCounter++;
                }
              },
              complete: () => {
                resolve();
              },
            });
          };
          sendRequest();
        });
      }
    } catch (e: any) {
      this.blockUI.stop(BLOCKUI_DEFAULT);

      this.commonService.displayErrorMessage(e.error.detail, 10000);
    }
  }
