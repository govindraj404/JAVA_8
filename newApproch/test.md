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

      const totalDataDone: any[] = [];
      const orderData: OrderData[] = [];
      const uploadPartsArray: UploadPart[] = [];
      let countParts = 0;
      const time: number = new Date().getTime();

      const concurrentLimit = 2;

      for (let index = 1; index < NUM_CHUNKS + 1; index += concurrentLimit) {
        const chunkPromises = [];

        for (let i = 0; i < concurrentLimit && index + i <= NUM_CHUNKS; i++) {
          const currentIndex = index + i;
          const start = (currentIndex - 1) * FILE_CHUNK_SIZE;
          const end = currentIndex * FILE_CHUNK_SIZE;
          const blob =
            currentIndex < NUM_CHUNKS ? file.slice(start, end) : file.slice(start);

          totalDataDone.push({
            part: currentIndex - 1,
            loaded: 0,
          });

          chunkPromises.push(this.uploadPart(currentIndex, blob, region, respFile, totalDataDone, fileSize, time));
        }

        await Promise.all(chunkPromises);
      }
    } catch (e) {
      console.log('error: ', e);
    }
  }

  private async uploadPart(
    index: number,
    blob: Blob,
    region: string,
    respFile: SendResponseFile,
    totalDataDone: any[],
    fileSize: number,
    time: number
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

      const uploadUrlPresigned = await this.preSignedPartUrl(this.partFile);

      if (uploadUrlPresigned) {
        const req = new HttpRequest('PUT', uploadUrlPresigned.filePartDto.presignedUrl, blob, {
          reportProgress: true,
        });

        await lastValueFrom(this.httpClient.request(req).pipe(take(1)));

        let percentDone = 0;
        totalDataDone[index - 1] = {
          part: index,
          loaded: blob.size,
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

        if (index === NUM_CHUNKS) {
          this.uploadCompleted$.next(this.successList);
        }
      }
    } catch (e) {
      console.log('error: ', e);
    }
  }

  // Remaining code
}
