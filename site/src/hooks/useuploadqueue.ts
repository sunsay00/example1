import { useState, useRef, useEffect } from 'react';
import { useS3Upload } from './uses3upload';

type UploadStatus = 'active' | 'queued' | 'paused' | 'error' | 'complete';

export type UploadItem = {
  readonly id: string,
  file: File,
  status: UploadStatus,
  sofar: number,
  total: number,
  err?: unknown
}

export const useUploadQueue = (opts?: {
  allowedTypes: string[],
  mbLimit: number,
  onUploadComplete?: (name: string, url: string) => void,
  onNameOverride?: (name: string) => string,
}) => {

  const { upload, cancel, pause, resume } = useS3Upload();

  const [_items, setItems] = useState<UploadItem[]>([]);
  const itemsRef = useRef(_items);
  itemsRef.current = _items;
  const counterRef = useRef(0);
  const [_activeItemId, setActiveItemId] = useState<string | undefined>(undefined);
  const activeItemIdRef = useRef(_activeItemId);
  activeItemIdRef.current = _activeItemId;

  const queueItem = (file: File) => {
    if (opts && opts.allowedTypes && !opts.allowedTypes.includes(file.type)) return;
    const item: UploadItem = {
      id: `${++counterRef.current}`,
      file,
      status: opts && opts.mbLimit != undefined && file.size > (1024 * 1024 * opts.mbLimit) ? 'error' : 'queued',
      sofar: 0, total: 0
    };
    setItems(v => [...v, item]);
    if (!activeItemIdRef.current)
      setActiveItemId(item.id);
  }

  const updateItem = (id: string, fields: Partial<Omit<UploadItem, 'id'>>) => {
    setItems(v => v.map(itm => itm.id == id ? { ...itm, ...fields } : itm));
  }

  const removeItem = (item: UploadItem) => {
    setItems(v => v.filter(itm => itm.id != item.id));
    if (item.id == activeItemIdRef.current) {
      cancel(item.file);
      const next = itemsRef.current.find(itm => itm.id != item.id && itm.status == 'queued');
      setActiveItemId(next && next.id);
    }
  }

  const toggleActiveItem = () => {
    const item = itemsRef.current.find(itm => itm.id == activeItemIdRef.current);
    if (item) {
      if (item.status == 'active') {
        updateItem(item.id, { status: 'paused' });
        pause(item.file);
      } else if (item.status == 'paused') {
        updateItem(item.id, { status: 'active' });
        resume(item.file);
      }
    }
  }

  useEffect(() => {
    if (activeItemIdRef.current) {
      const queuedItem = itemsRef.current.find(itm => itm.id == activeItemIdRef.current && itm.status == 'queued');
      if (queuedItem) {
        updateItem(queuedItem.id, { status: 'active' });
        upload(queuedItem.file, {
          nameOverride: opts && opts.onNameOverride && opts.onNameOverride(queuedItem.file.name),
          onProgressChange: (sofar, total) => {
            activeItemIdRef.current && updateItem(activeItemIdRef.current, { sofar, total });
          }
        })
          .then(url => {
            activeItemIdRef.current && updateItem(activeItemIdRef.current, { status: 'complete' });
            opts && opts.onUploadComplete && opts.onUploadComplete(queuedItem.file.name, url);
          })
          .catch(err => {
            console.error(err);
            activeItemIdRef.current && updateItem(activeItemIdRef.current, { status: 'error', err });
          })
          .finally(() => {
            const next = itemsRef.current.find(i => i.status == 'queued');
            setActiveItemId(next && next.id);
          });
      }
    }
  }, [activeItemIdRef.current]);

  return { items: itemsRef.current, queueItem, removeItem, toggleActiveItem };
}
