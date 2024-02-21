```typescript
// limited-concurrency-observable.ts

import { Subject, Observable } from 'rxjs';

export class LimitedConcurrencyObservable {
  private queue: (() => void)[] = [];
  private activeCount = 0;
  private concurrency: number;
  private subject: Subject<void>;

  constructor(concurrency: number) {
    this.concurrency = concurrency;
    this.subject = new Subject();
    this.startProcessing();
  }

  private startProcessing() {
    while (this.activeCount < this.concurrency && this.queue.length > 0) {
      const task = this.queue.shift();
      this.activeCount++;

      // Execute the task and notify the subject when done
      task?.();
      this.activeCount--;
      this.startProcessing();
      this.subject.next();
    }
  }

  enqueue(task: () => void) {
    this.queue.push(task);
    this.subject.next(); // Start processing if there is room in the queue
  }

  asObservable(): Observable<void> {
    return this.subject.asObservable();
  }
}
