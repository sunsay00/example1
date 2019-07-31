export const useLocale = (): string | undefined => {
  const locale = navigator.language || (navigator as any).browserLanguage || ([...navigator.languages] || ["en"])[0];
  return locale;
}

