export declare type Group = 'guests' | 'testers' | 'buyers' | 'sales' | 'dealership' | 'admins' | '__postconfirm__';
export declare const grant: (groups: Group[]) => (target: any, propertyKey: string, descriptor: TypedPropertyDescriptor<(...params: any[]) => any>) => void;
//# sourceMappingURL=grant.d.ts.map