```typescript
import { Injectable } from '@angular/core';
import { HttpClient, HttpEvent, HttpEventType, HttpRequest, HttpResponse } from '@angular/common/http';
import { Observable, Subject, forkJoin } from 'rxjs';
import { mergeMap } from 'rxjs/operators';
import { OrderData, PartFile, SendResponseFile, ToBeUploadedFile, UploadPart } from 'src/app/feature/send-file/_models.ts/sendFile';

@Injectable({
  providedIn: 'root',
})
export class UploadService {
  private uploadProgress$ = new Subject<any>();
  private uploadCompleted$ = new Subject<File[]>();

  constructor(private httpClient: HttpClient) {}

  getUploadProgress$(): Observable<any> {
    return this.uploadProgress$.asObservable();
  }

  getUploadCompleted$(): Observable<File[]> {
    return this.uploadCompleted$.asObservable();
  }

  async preSignedPartUrl(files: PartFile): Promise<any> {
    return this.httpClient.post(`${this.url}/presignPart`, files).toPromise();
  }

  uploadMultipleFiles(toBeUploadedFilesList: ToBeUploadedFile[]): Observable<any> {
    const uploadObservables: Observable<any>[] = [];
    const successList: File[] = [];

    toBeUploadedFilesList.forEach((toBeUploadedFileObj: ToBeUploadedFile) => {
      const uploadObservable = this.uploadMultipartFile(
        toBeUploadedFileObj.file,
        toBeUploadedFileObj.region,
        toBeUploadedFileObj.respFile
      );
      uploadObservables.push(uploadObservable);
    });

    return forkJoin(uploadObservables);
  }

  private async uploadMultipartFile(file: File, region: string, respFile: SendResponseFile): Promise<void> {
    try {
      const FILE_CHUNK_SIZE = 50 * 1000 * 1000; // 50MB
      const fileSize = file.size;
      const NUM_CHUNKS = Math.floor(fileSize / FILE_CHUNK_SIZE) + 1;
      const uploadPartsArray: UploadPart[] = [];
      const orderData: OrderData[] = [];
      const time: number = new Date().getTime();

      for (let index = 1; index < NUM_CHUNKS + 1; index++) {
        const start = (index - 1) * FILE_CHUNK_SIZE;
        const end = index * FILE_CHUNK_SIZE;
        const blob = index < NUM_CHUNKS ? file.slice(start, end) : file.slice(start);

        const partFile: PartFile = {
          filePartDto: {
            region: region,
            bucket: respFile.bucket,
            uploadId: respFile.uploadId,
            fileObjectKey: respFile.fileObjectKey,
            partNumber: index.toString(),
            contentLength: blob.size,
          },
        };

        const uploadUrlPresigned = await this.preSignedPartUrl(partFile);

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
                this.handleUploadProgress(event, time, fileSize, file);
                break;
              case HttpEventType.Response:
                this.handleUploadResponse(event, orderData, uploadPartsArray, NUM_CHUNKS, region, respFile, successList, file);
                break;
            }
          });
        }
      }
    } catch (e) {
      console.log('error: ', e);
    }
  }

  private handleUploadProgress(event: HttpEvent<any>, time: number, fileSize: number, file: File): void {
    const percentDone = this.calculatePercentDone(event.loaded, fileSize);
    const diff = new Date().getTime() - time;
    this.uploadProgress$.next({
      progress: percentDone,
      token: respFile.tempStrId,
      currentPart: index,
      totalSize: fileSize,
      fileName: file.name,
      speed: Math.round((event.loaded / diff) * 1000),
    });
  }

  private calculatePercentDone(loaded: number, totalSize: number): number {
    return Math.round((loaded / totalSize) * 100);
  }

  private handleUploadResponse(
    event: HttpEvent<any>,
    orderData: OrderData[],
    uploadPartsArray: UploadPart[],
    NUM_CHUNKS: number,
    region: string,
    respFile: SendResponseFile,
    successList: File[],
    file: File
  ): void {
    if (event instanceof HttpResponse) {
      const currentPresigned = orderData.find((item) => item.presignedUrl === event.url);

      uploadPartsArray.push({
        etag: event.headers.get('ETag')?.replace(/[|&;$%@"<>()+,]/g, ''),
        partNumber: currentPresigned?.index,
      });

      if (uploadPartsArray.length === NUM_CHUNKS) {
        this.completeMultipartUpload(uploadPartsArray, region, respFile, successList, file);
      }
    }
  }

  private completeMultipartUpload(
    uploadPartsArray: UploadPart[],
    region: string,
    respFile: SendResponseFile,
    successList: File[],
    file: File
  ): void {
    lastValueFrom(
      this.httpClient.post(`${this.url}/completeMultipart`, {
        file: {
          region: region,
          bucket: respFile.bucket,
          fileId: respFile.fileId,
          fileObjectKey: respFile.fileObjectKey,
          completedPartList: uploadPartsArray.sort((a, b) => a.partNumber - b.partNumber),
          uploadId: respFile.uploadId,
        },
      })
    ).then((res) => {
      successList.push(file);
      if (successList.length === this.uploadFileCount) {
        this.uploadCompleted$.next(successList);
      }
    });
  }
}

