# -*- mode: python ; coding: utf-8 -*-


a = Analysis(
    ['tools/pat_visual_sim.py'],
    pathex=[],
    binaries=[],
    datas=[('src', 'pat/src'), ('extensions', 'pat/extensions'), ('docs', 'pat/docs'), ('tools', 'pat/tools')],
    hiddenimports=[],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    noarchive=False,
    optimize=0,
)
pyz = PYZ(a.pure)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.datas,
    [],
    name='PAT_Sim',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    upx_exclude=[],
    runtime_tmpdir=None,
    console=False,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
)
app = BUNDLE(
    exe,
    name='PAT_Sim.app',
    icon=None,
    bundle_identifier=None,
)
