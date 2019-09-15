export type JSONValue = string | number | boolean | JSONObject | JSONArray;
export type JSONObject = { [_: string]: JSONValue; }
export interface JSONArray extends Array<JSONValue> { }

export type Variables = { [_: string]: JSONValue | undefined; }

export type Dict<T> = {
  [_: string]: T | undefined;
  [_: number]: T | undefined;
}

export type Cursorize = {
  cursor?: string,
};

export type Cursored<T> = {
  cursor?: string,
  data: T,
}

export type Point = {
  lon: number,
  lat: number,
};

export type IUserContext = {
  sub: string,
  groups?: string[],
};

export type Paginated<T> = {
  cursor?: string,
  items: T[],
};
