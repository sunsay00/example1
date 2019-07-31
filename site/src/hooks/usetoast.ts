export const useToast = () => {
  return {
    info: (msg: string) => {
      console.log(msg);
    },
    error: (msg: string) => {
      console.error(msg);
    },
    success: (msg: string) => {
      console.log(msg);
    }
  };
}

