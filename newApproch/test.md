```typescript

import { Injectable } from '@angular/core';
import { HttpClient, HttpEvent, HttpEventType, HttpRequest, HttpResponse } from '@angular/common/http';
import { Observable, forkJoin, from, of } from 'rxjs';
import { lastValueFrom, mergeMap, take } from 'rxjs/operators';

import { OrderData, PartFile, SendResponseFile, ToBeUploadedFile, UploadPart } from 'src/app/feature/send-file/_models.ts/sendFile';

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

  constructor(private httpClient: HttpClient) {}

  async preSignedPartUrl(files: PartFile): Promise<any> {
    return lastValueFrom(this.httpClient.post(`${this.url}/presignPart`, files));
  }

  async uploadMultipartFile(file: File, region: string, respFile: SendResponseFile) {
    try {
      const FILE_CHUNK_SIZE = 50 * 1000 * 1000; // 10MB
      const fileSize = file.size;
      const NUM_CHUNKS = Math.floor(fileSize / FILE_CHUNK_SIZE) + 1;

      const concurrentLimit = 2; // Set your desired concurrency limit

      const totalDataDone: any[] = [];
      const orderData: OrderData[] = [];
      const uploadPartsArray: UploadPart[] = [];
      let countParts = 0;
      const time: number = new Date().getTime();

      for (let index = 1; index < NUM_CHUNKS + 1; index += concurrentLimit) {
        const chunkPromises = [];

        for (let i = 0; i < concurrentLimit && index + i <= NUM_CHUNKS; i++) {
          const partIndex = index + i;
          const start = (partIndex - 1) * FILE_CHUNK_SIZE;
          const end = partIndex * FILE_CHUNK_SIZE;
          const blob = partIndex < NUM_CHUNKS ? file.slice(start, end) : file.slice(start);

          totalDataDone.push({
            part: partIndex - 1,
            loaded: 0,
          });

          chunkPromises.push(this.uploadPart(partIndex, blob, region, respFile));
        }

        await Promise.all(chunkPromises);
      }
    } catch (e) {
      console.error('Error uploading file:', e);
    }
  }

  private async uploadPart(index: number, blob: Blob, region: string, respFile: SendResponseFile): Promise<void> {
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

      const uploadUrlPresigned = await this.preSignedPartUrl(this.partFile);

      if (uploadUrlPresigned) {
        const req = new HttpRequest('PUT', uploadUrlPresigned.filePartDto.presignedUrl, blob, {
          reportProgress: true,
        });

        const event: HttpEvent<any> = await this.httpClient.request(req).toPromise();

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
            if (event instanceof HttpResponse) {
              // Handle completion logic as needed
              console.log(`Part ${index} completed successfully.`);
            }
            break;
        }
      }
    } catch (error) {
      console.error(`Error uploading part ${index}:`, error);
    }
  }

  // ... Remaining code ...
}
