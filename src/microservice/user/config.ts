export default {
   name: 'user',
   patterns: {
      signIn: 'user.auth.signIn',
      signUp: 'user.auth.signUp',
      refreshToken: 'user.auth.refreshToken',
      verifyToken: 'user.auth.verifyToken',
      sendResetPasswordCode: 'user.auth.sendResetPasswordCode',
      resetPassword: 'user.auth.resetPassword',
      verifyAccount: 'user.auth.verifyAccount',
      userCRUD: 'user.user.CRUD',
      groupCRUD: 'user.group.CRUD',
      roleCRUD: 'user.role.CRUD',
   },
   permissions: {
      admin: {
         scope: 'user.admin.scope',
      },

      group: {
         create: 'user.group.create',
         read: 'user.group.read',
         update: 'user.group.update',
         delete: 'user.group.delete',
      },
      role: {
         create: 'user.role.create',
         read: 'user.role.read',
         update: 'user.role.update',
         delete: 'user.role.delete',
      },
      user: {
         create: 'user.user.create',
         read: 'user.user.read',
         update: 'user.user.update',
         delete: 'user.user.delete',
      },
   },
   jwt: {
      secret: `5Y$Vn2PxU&)[).5M?52:$Y1Fsmha=P#[VrqME=yB,V4Q=:3EE(hxE#}/Hs|'NGr`,
      accessExpiresInMinutes: Number(process.env.JWT_EXPIRES_IN_MINUTES || 60),
      refreshExpiresInMinutes: Number(process.env.JWT_EXPIRES_IN_MINUTES || 60) + 15,
   },
   httpWebVerifyAccountUrl: process.env.HTTP_WEB_VERIFY_ACCOUNT_URL || '#',
   httpWebVerifyResetPwdUrl: process.env.HTTP_WEB_VERIFY_RESET_PWD_URL || '#',
} as const;
