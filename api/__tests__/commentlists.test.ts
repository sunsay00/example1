import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, User, CommentItemInput } from '@inf/cf-gen';
import vars from '../src/_vars';

const config = verifyVars({
  STAGE: process.env.STAGE,
  AWS_REGION: process.env.AWS_REGION
});

const db = new RDSDBClient(vars.DB_TEST_URL);
const cache = undefined; // new CacheClient(config.redisUrl),

const resolver = createDefaultResolver({
  stage: config.STAGE,
  region: config.AWS_REGION,
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache,
});

describe('commentlists', () => {
  let userCtx: IUserContext;
  let user: User;
  beforeAll(async () => {
    await db.init();
    userCtx = { sub: 'commenter', groups: [] };
    user = await fix.backendCreateUser(resolver.store(), userCtx, "the user", "avataruri.jpg", "en", "us-east-1");
  });

  afterAll(async () => {
    await fix.backendRemoveUser(resolver.store(), userCtx, user.id);
    await db.deinit();
  });

  it('should CRUD a comment list', async () => {
    let comments: CommentItemInput[] = [{ text: 'mytext1' }, { text: 'mytext1' }];
    const { id, ...rest } = await fix.commentListsCreate(resolver, userCtx, 'myparentid', comments);
    const commentList = {
      ...rest, comments: rest.comments.map(c => {
        const { createdAt, updatedAt, ...rest } = c;
        return rest;
      })
    };
    expect(commentList).toMatchSnapshot();
    await fix.commentListsRemove(resolver, userCtx, id);
  });

  it('should return undefined if a comment is added to a non-existant list', async () => {
    const comment: CommentItemInput = {
      text: 'mytext1',
    };
    const appendedComment = await fix.commentListsAppendComment(resolver, userCtx, "5896757689", comment);
    expect(appendedComment).toBeUndefined();
  });

  it('should append a comment to an existing list', async () => {
    const comment: CommentItemInput = {
      text: 'mytext11',
    };
    const commentList = await fix.commentListsCreate(resolver, userCtx, 'myparentid2', [comment]);
    expect(commentList.comments).toHaveLength(1);
    const anotherComment: CommentItemInput = {
      text: 'mytext22',
    };
    const appendedComment = await fix.commentListsAppendComment(resolver, userCtx, commentList.id, anotherComment);
    if (appendedComment == undefined) throw new Error('appended comment not found');
    const { createdAt, updatedAt, ...appendedCommentRest } = appendedComment;
    expect(appendedCommentRest).toMatchSnapshot();
    const foundCommentList = await fix.commentListsFindById(resolver, userCtx, commentList.id);
    expect(foundCommentList).toBeDefined();
    if (foundCommentList != undefined) {
      expect(foundCommentList.comments).toHaveLength(2);
    }

    anotherComment.text = 'mytext33';
    const updatedComment = await fix.commentListsUpdateComment(resolver, userCtx, commentList.id, 1, anotherComment);
    if (updatedComment != undefined) {
      const { createdAt: _1, updatedAt: _2, ...updatedCommentRest } = updatedComment;
      expect(updatedCommentRest).toMatchSnapshot();
    }
    const foundCommentList2 = await fix.commentListsFindById(resolver, userCtx, commentList.id);
    expect(foundCommentList2).toBeDefined();
    if (foundCommentList2 != undefined) {
      expect(foundCommentList2.comments).toHaveLength(2);
      expect(foundCommentList2.comments[0].text).toEqual(comment.text);
      expect(foundCommentList2.comments[1].text).toEqual(anotherComment.text);
    }

    expect(await fix.commentListsRemove(resolver, userCtx, commentList.id)).toBeDefined();
  });

});

