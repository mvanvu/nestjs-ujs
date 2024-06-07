export default {
   name: 'user',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      refreshToken: 'user.refreshToken',
      verifyToken: 'user.verifyToken',
      sendResetPasswordCode: 'user.updateResetPasswordCode',
      resetPassword: 'user.resetPassword',
      verifyAccount: 'user.verifyAccount',
      userCRUD: 'user.CRUD',
      groupCRUD: 'group.CRUD',
      roleCRUD: 'role.CRUD',
   },
   permissions: {
      group: {
         create: 'group.create',
         read: 'group.read',
         update: 'group.update',
         delete: 'group.delete',
      },
      role: {
         create: 'role.create',
         read: 'role.read',
         update: 'role.update',
         delete: 'role.delete',
      },
      user: {
         create: 'user.create',
         read: 'user.read',
         update: 'user.update',
         delete: 'user.delete',
      },
   },
   jwt: {
      secret: `5Y$Vn2PxU&)[).5M?52:$Y1Fsmha=P#[VrqME=yB,V4Q=:3EE(hxE#}/Hs|'NGr`,
      accessExpiresInMinutes: 60,
      refreshExpiresInMinutes: 75,
   },
   rootUID: '662c7a7a2fc319b3c782be7d',
   httpWebVerifyAccountUrl: process.env.HTTP_WEB_VERIFY_ACCOUNT_URL || '#',
   httpWebVerifyResetPwdUrl: process.env.HTTP_WEB_VERIFY_RESET_PWD_URL || '#',
} as const;
