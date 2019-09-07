module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  transform: {
    '^.+\\.tsx?$': 'ts-jest',
  },
  testPathIgnorePatterns: ["/build/"],
  globals: {
    'ts-jest': {
      tsConfig: false
    }
  }
};
