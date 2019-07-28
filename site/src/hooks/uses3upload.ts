import { useRef } from 'react';
import { useApi } from './useapi';
import { has } from 'common';

const PART_SIZE = 5 * 1024 * 1024; //minimum part size defined by aws s3
const RETRY_WAIT_SEC = 30; //wait before retrying again on upload failure

class SendToS3Error { };

type FileInfo = {
  name: string,
  type: string,
  size: string,
  lastModifiedDate: string,
}

type Job = {
  uploadXHR: XMLHttpRequest | undefined,
  file: File,
  get: Get,
  sendBackData?: { Key: string, UploadId: string },
  onUploadComplete?: (data: { Location: string }) => void,
  onProgressChange?: (i: number, n: number) => void,
  progress: number[],
  isPaused: boolean,
  currentPartNum: number,
  currentStart: number,
};

type Get = (command: string, payload: string) => Promise<string>;

class S3Upload {
  private get: Get;
  //private serverUrl: string;
  private job: Job | undefined;

  constructor(get: Get) {
    this.get = get;
  }

  start = async (
    file: File,
    opts: {
      nameOverride?: string,
      onProgressChange?: (i: number, n: number) => void
    }) => new Promise<string>(async (resolve, reject) => {
      try {
        if (this.job) return;

        const job: Job = {
          uploadXHR: undefined,
          file,
          get: this.get,
          sendBackData: undefined,
          onUploadComplete: data => resolve(data.Location),
          onProgressChange: opts.onProgressChange,
          progress: [],
          isPaused: false,
          currentPartNum: 0,
          currentStart: 0,
        };
        this.job = job;

        const fileInfo = {
          name: (opts.nameOverride || file.name).replace(/[^\w\d_\-\.]+/ig, ''),
          type: file.type,
          size: `${file.size}`,
          lastModifiedDate: (file as any).lastModifiedDate,
        };

        const data: unknown = JSON.parse(await S3Upload.createMultipartUpload(job.get, fileInfo, file.name));
        if (!has(data, 'Key', 'string') || !has(data, 'UploadId', 'string'))
          throw new Error('invalid response');
        job.sendBackData = data;

        await S3Upload.uploadPart(job, 0, 0);
      } catch (err) {
        if (err instanceof SendToS3Error) {
          setTimeout(() => {
            this.retry();
          }, RETRY_WAIT_SEC * 1000);
        } else {
          reject(err);
        }
      }
    })

  pause = () => {
    if (!this.job) return;
    if (this.job.isPaused) return;
    this.job.isPaused = true;
    this.job.uploadXHR && this.job.uploadXHR.abort();
    this.job.uploadXHR = undefined;
  }

  resume = () => {
    if (!this.job) return;
    if (!this.job.isPaused) return;
    this.job.isPaused = false;
    S3Upload.uploadPart(this.job, this.job.currentPartNum, this.job.currentStart);
  }

  cancel = async () => {
    if (!this.job) return;
    this.pause();
    console.log(this.job.sendBackData);
    await this.get('AbortMultipartUpload', JSON.stringify(this.job.sendBackData));
  }

  retry = () => {
    if (!this.job) return;
    if (this.job.isPaused) return;
    if (!this.job.uploadXHR) return;
    S3Upload.uploadPart(this.job, this.job.currentPartNum, this.job.currentStart);
  }

  private static createMultipartUpload = async (get: Get, fileInfo: FileInfo, name: string) => {
    return await get('CreateMultipartUpload', JSON.stringify({ Key: fileInfo.name, ContentType: fileInfo.type, name }));
  }

  private static uploadPart = async (job: Job, partNum: number, start: number) => {
    job.currentStart = start;
    job.currentPartNum = partNum;

    if (start >= job.file.size) {
      await S3Upload.completeMultipartUpload(job);
    } else {
      const end = Math.min(start + PART_SIZE, job.file.size);
      const blob = job.file.slice(start, end);

      const signed: unknown = JSON.parse(await job.get('SignUploadPart', JSON.stringify({
        ...job.sendBackData,
        PartNumber: partNum + 1
      })));
      if (!has(signed, 'url', 'string'))
        throw new Error('bad arg');

      await S3Upload.sendToS3(job, signed.url, blob, partNum, start);

      await S3Upload.uploadPart(job, partNum + 1, end);
    }
  }

  private static sendToS3 = (job: Job, signedUrl: string, blob: Blob, index: number, start: number) => {
    return new Promise<void>((resolve, reject) => {
      job.progress[index] = 0;
      const request = job.uploadXHR = new XMLHttpRequest();
      request.onreadystatechange = () => {
        if (request.readyState === 4) {
          job.uploadXHR = undefined;
          if (request.status !== 200) {
            S3Upload.updateProgressBar(job);
            !job.isPaused && reject(new SendToS3Error());
          } else {
            job.progress[index] = blob.size;
            S3Upload.updateProgressBar(job);
            resolve();
          }
        }
      };
      request.upload.onprogress = (e) => {
        if (e.lengthComputable) {
          job.progress[index] = e.loaded;
          S3Upload.updateProgressBar(job);
        }
      };
      request.open('PUT', signedUrl, true);
      request.setRequestHeader('contentLength', `${blob.size}`);
      request.setRequestHeader('Accept', 'text/html');
      request.overrideMimeType('text/plain; charset=x-user-defined');
      request.send(blob);
    });
  }

  private static completeMultipartUpload = async (job: Job) => {
    const data: unknown = JSON.parse(await job.get('CompleteMultipartUpload', JSON.stringify(job.sendBackData)));
    if (!has(data, 'Location', 'string')) throw new Error('invalid response');
    if (!data.Location) throw new Error('failed to complete multi-part upload');
    job.onUploadComplete && job.onUploadComplete(data);
  }

  private static updateProgressBar = (job: Job) => {
    if (!job.onProgressChange) return;

    if (job.progress.length == 0) return;
    let sofar = 0;
    for (let i = 0; i < job.progress.length; i++)
      sofar += job.progress[i];

    job.onProgressChange(sofar, job.file.size);
  }
}

export const useS3Upload = () => {
  const { multipartUpload } = useApi();
  const uploaderRef = useRef<{ [key: string]: S3Upload }>({});
  return {
    upload: async (file: File, opts?: {
      onProgressChange?: (i: number, n: number) => void,
      nameOverride?: string
    }) => {
      if (uploaderRef.current[file.name])
        throw new Error('already uploaded');
      const up = new S3Upload(multipartUpload);
      uploaderRef.current[file.name] = up;
      try {
        return await up.start(file, {
          nameOverride: opts && opts.nameOverride,
          onProgressChange: opts && opts.onProgressChange,
        });
      } finally {
        delete uploaderRef.current[file.name];
      }
    },
    cancel: (file: File) => {
      uploaderRef.current[file.name].cancel();
      delete uploaderRef.current[file.name];
    },
    pause: (file: File) => uploaderRef.current[file.name].pause(),
    resume: (file: File) => uploaderRef.current[file.name].resume(),
    retry: (file: File) => uploaderRef.current[file.name].retry(),
  };
}