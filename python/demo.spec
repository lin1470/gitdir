# -*- mode: python -*-

block_cipher = None


a = Analysis(['demo.py'],
             pathex=['/home/bruce/gitdir/python'],
             binaries=[],
             datas=[],
             hiddenimports=[],
             hookspath=[],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='demo',
          debug=False,
          strip=False,
          upx=True,
          console=True )