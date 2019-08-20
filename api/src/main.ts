export const main = async (version: number, headers: { [_: string]: string }, query: string, variables: string, user: {}) => {
  console.log('Main Main');

  return {
    errors: [] as Error[],
    data: { no: 'wae' }
  }
}






