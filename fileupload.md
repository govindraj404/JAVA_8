```typescript

import { Injectable } from '@angular/core';
import { HttpClient, HttpEvent, HttpEventType, HttpRequest, HttpResponse } from '@angular/common/http';
import { Observable, from, Subject } from 'rxjs';
import { lastValueFrom, mergeMap } from 'rxjs/operators';

// Existing imports...

@Injectable({
  providedIn: 'root',
})
export class UploadService {
  uploadProgress$ = new Subject<any>();
  finishedProgress$ = new Subject<any>();
  uploadCompleted$ = new Subject<any>();
  files!: File[];
  successList: File[] = [];
  uploadFileCount!: number;
  url: string = '/api/uploadFile';
  partFile!: PartFile;

  async preSignedPartUrl(files: PartFile): Promise<any> {
    return lastValueFrom(
      this.httpClient.post(`${this.url}/presignPart`, files)
    );
  }

  async uploadMultipartFile(
    file: File,
    region: string,
    respFile: SendResponseFile
  ) {
    try {
      const FILE_CHUNK_SIZE = 50 * 1000 * 1000; // 10MB
      const fileSize = file.size;
      const NUM_CHUNKS = Math.floor(fileSize / FILE_CHUNK_SIZE) + 1;

      const concurrency = 2; // Set your desired concurrency level

      const limitedObservable = {
        queue: [] as (() => void)[],
        activeCount: 0,
        concurrency,
        subject: new Subject<void>(),
        startProcessing() {
          while (this.activeCount < this.concurrency && this.queue.length > 0) {
            const task = this.queue.shift();
            this.activeCount++;

            // Execute the task and notify the subject when done
            task?.();
            this.activeCount--;
            this.startProcessing();
            this.subject.next();
          }
        },
        enqueue(task: () => void) {
          this.queue.push(task);
          this.subject.next(); // Start processing if there is room in the queue
        },
        asObservable(): Observable<void> {
          return this.subject.asObservable();
        },
      };

      for (let index = 1; index < NUM_CHUNKS + 1; index++) {
        const start = (index - 1) * FILE_CHUNK_SIZE;
        const end = index * FILE_CHUNK_SIZE;
        const blob = index < NUM_CHUNKS ? file.slice(start, end) : file.slice(start);

        totalDataDone.push({
          part: index - 1,
          loaded: 0,
        });

        // Wrap the loop body in a function and enqueue it with limitedObservable
        limitedObservable.enqueue(async () => {
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

          if (uploadUrlPresigned) {
            orderData.push({
              presignedUrl: uploadUrlPresigned.filePartDto.presignedUrl,
              index: index,
            });

            const req = new HttpRequest(
              'PUT',
              uploadUrlPresigned.filePartDto.presignedUrl,
              blob,
              { reportProgress: true }
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
              }

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
                      this.uploadCompleted$.next(this.successList);
                    }
                  });
                }
              }
            });
          }
        });
      }

      // Wait for all tasks to complete before proceeding
      await limitedObservable.asObservable().toPromise();
    } catch (e) {
      console.log('error: ', e);
    }
  }
  
  // ... Remaining existing code ...
}
