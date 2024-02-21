
```typescript

import { EventEmitter, Injectable } from '@angular/core';

import {

  HttpClient,

  HttpEvent,

  HttpEventType,

  HttpRequest,

  HttpResponse,

} from '@angular/common/http';

import { Observable, from, lastValueFrom, mergeMap } from 'rxjs';

import {

  OrderData,

  PartFile,

  SendResponseFile,

  ToBeUploadedFile,

  UploadPart,

} from 'src/app/feature/send-file/_models.ts/sendFile';
@Injectable({

  providedIn: 'root',

})

export class UploadService {

  uploadProgress$ = new EventEmitter<any>();

  finishedProgress$ = new EventEmitter<any>();

  uploadCompleted$ = new EventEmitter<any>();

  files!: File[];

  successList: File[] = [];

  uploadFileCount!: number;

  url: string = '/api/uploadFile';

  partFile!: PartFile;

 

  constructor(private httpClient: HttpClient) {}

 

  async preSignedPartUrl(files: PartFile): Promise<any> {

    return lastValueFrom(

      this.httpClient.post(`${this.url}/presignPart`, files)

    );

  }
uploadMultipleFiles(

    toBeUploadedFilesList: ToBeUploadedFile[]

  ): Observable<any> {

    this.successList = [];

    this.uploadFileCount = toBeUploadedFilesList.length;

    return from(toBeUploadedFilesList).pipe(

      mergeMap((toBeUploadedFileObj: ToBeUploadedFile) => {

        return this.uploadMultipartFile(

          toBeUploadedFileObj.file,

          toBeUploadedFileObj.region,

          toBeUploadedFileObj.respFile

        );

      }, 2)

    );

  }
async uploadMultipartFile(

    file: File,

    region: string,

    respFile: SendResponseFile

  ) {

    try {

      //   const FILE_CHUNK_SIZE = 10000000; // 10MB

      const FILE_CHUNK_SIZE = 50 * 1000 * 1000; // 10MB

      const fileSize = file.size;

      const NUM_CHUNKS = Math.floor(fileSize / FILE_CHUNK_SIZE) + 1;

      let start, end, blob;

      let uploadPartsArray: UploadPart[] = [];

      let countParts = 0;

      let orderData: OrderData[] = [];

      let totalDataDone: any[] = [];

      const time: number = new Date().getTime();

      let speed = 0;

      for (let index = 1; index < NUM_CHUNKS + 1; index++) {

        start = (index - 1) * FILE_CHUNK_SIZE;

        end = index * FILE_CHUNK_SIZE;

        blob = index < NUM_CHUNKS ? file.slice(start, end) : file.slice(start);

        totalDataDone.push({

          part: index - 1,

          loaded: 0,

        });
 // (1) Generate presigned URL for each part

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

        const uploadUrlPresigned = await this.preSignedPartUrl(this.partFile);

        // (2) Puts each file part into the storage server

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
this.httpClient.request(req).subscribe((event: HttpEvent<any>) => {

            switch (event.type) {

              case HttpEventType.UploadProgress:

                let percentDone = 0;

                totalDataDone[index - 1] = {

                  part: index,

                  loaded: event.loaded,

                };

                totalDataDone.forEach((a) => (percentDone += a.loaded));

                const diff = new Date().getTime() - time;

                this.uploadProgress$.emit({

                  progress: Math.round((percentDone / fileSize) * 100),

                  token: respFile.tempStrId,

                  currentPart: index,

                  totalSize: fileSize,

                  fileName: file.name,

                  speed: Math.round((percentDone / diff) * 1000),

                });

                break;

              case HttpEventType.Response:

            }
// (3) Calls the CompleteMultipartUpload endpoint in the backend server

            if (event instanceof HttpResponse) {

              const currentPresigned = orderData.find(

                (item) => item.presignedUrl === event.url

              );

              countParts++;

              uploadPartsArray.push({

                etag: event.headers

                  .get('ETag')

                  ?.replace(/[|&;$%@"<>()+,]/g, ''),

                partNumber: currentPresigned?.index,

              });

 

              if (uploadPartsArray.length === NUM_CHUNKS) {

                lastValueFrom(

                  this.httpClient.post(`${this.url}/completeMultipart`, {
                  file: {

                      region: region,

                      bucket: respFile.bucket,

                      fileId: respFile.fileId,

                      fileObjectKey: respFile.fileObjectKey,

                      completedPartList: uploadPartsArray.sort((a, b) => {

                        return a.partNumber - b.partNumber;

                      }),

                      uploadId: respFile.uploadId,

                    },

                  })

                ).then((res) => {

                  this.successList.push(file);

                  if (this.successList.length === this.uploadFileCount) {

                    this.uploadCompleted$.emit(this.successList);

                  }

                });

              }

            }

          });

        }

      }

    } catch (e) {

      console.log('error: ', e);

    }

  }

}
          

 
 

