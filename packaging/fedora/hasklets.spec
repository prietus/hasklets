# Haskell packaging guidelines:
# https://docs.fedoraproject.org/en-US/packaging-guidelines/Haskell/

%global pkg_name hasklets
%global pkgver %{pkg_name}-%{version}

# Hackage libraries hasklets needs that Fedora does not package (or not in
# a version that builds with Fedora's haskell-gi 0.26). They are built as
# ghc-* subpackages of this one.
%global tzdata tzdata-0.2.20260911.0
%global tz tz-0.1.3.6
%global gicairorender gi-cairo-render-0.1.2
%global gicairoconnector gi-cairo-connector-0.1.1
%global gipangocairo gi-pangocairo-1.0.31
%global gigtk4layershell gi-gtk4-layer-shell-0.1.0
%global subpkgs %{tzdata} %{tz} %{gicairorender} %{gicairoconnector} %{gipangocairo} %{gigtk4layershell}

%{?haskell_setup}
# The bundled libraries are only linked into the hasklets binary: no
# profiling or documentation subpackages for them.
%undefine with_ghc_prof
%undefine with_haddock

Name:           %{pkg_name}
Version:        0.1.2.0
Release:        1%{?dist}
Summary:        Desktop widgets for Wayland in Haskell

License:        BSD-3-Clause
URL:            https://github.com/prietus/hasklets
Source0:        %{url}/archive/v%{version}/%{pkgver}.tar.gz
Source1:        https://hackage.haskell.org/package/%{tzdata}/%{tzdata}.tar.gz
Source2:        https://hackage.haskell.org/package/%{tz}/%{tz}.tar.gz
Source3:        https://hackage.haskell.org/package/%{gicairorender}/%{gicairorender}.tar.gz
Source4:        https://hackage.haskell.org/package/%{gicairoconnector}/%{gicairoconnector}.tar.gz
Source5:        https://hackage.haskell.org/package/%{gipangocairo}/%{gipangocairo}.tar.gz
Source6:        https://hackage.haskell.org/package/%{gigtk4layershell}/%{gigtk4layershell}.tar.gz
# Hackage revisions of the .cabal files (newer dependency bounds).
Source12:       https://hackage.haskell.org/package/%{tz}/revision/10.cabal#/tz-10.cabal
Source14:       https://hackage.haskell.org/package/%{gicairoconnector}/revision/1.cabal#/gi-cairo-connector-1.cabal

BuildRequires:  ghc-rpm-macros-extra
BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-aeson-devel
BuildRequires:  ghc-array-devel
BuildRequires:  ghc-base-devel
BuildRequires:  ghc-binary-devel
BuildRequires:  ghc-bytestring-devel
BuildRequires:  ghc-containers-devel
BuildRequires:  ghc-data-default-devel
BuildRequires:  ghc-deepseq-devel
BuildRequires:  ghc-directory-devel
BuildRequires:  ghc-filepath-devel
BuildRequires:  ghc-gi-cairo-devel
BuildRequires:  ghc-gi-gdk4-devel
BuildRequires:  ghc-gi-gdkpixbuf-devel
BuildRequires:  ghc-gi-gio-devel
BuildRequires:  ghc-gi-glib-devel
BuildRequires:  ghc-gi-gobject-devel
BuildRequires:  ghc-gi-graphene-devel
BuildRequires:  ghc-gi-gtk4-devel
BuildRequires:  ghc-gi-pango-devel
BuildRequires:  ghc-haskell-gi-base-devel
BuildRequires:  ghc-haskell-gi-devel
BuildRequires:  ghc-haskell-gi-overloading-devel
BuildRequires:  ghc-mtl-devel
BuildRequires:  ghc-process-devel
BuildRequires:  ghc-template-haskell-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-time-devel
BuildRequires:  ghc-transformers-devel
BuildRequires:  ghc-unix-devel
BuildRequires:  ghc-utf8-string-devel
BuildRequires:  ghc-vector-devel
BuildRequires:  ghc-xml-devel
BuildRequires:  pkgconfig(cairo)
BuildRequires:  pkgconfig(cairo-gobject)
BuildRequires:  pkgconfig(gtk4-layer-shell-0)
BuildRequires:  pkgconfig(pangocairo)
BuildRequires:  gobject-introspection-devel
# gi-cairo-render
BuildRequires:  c2hs

Recommends:     curl

%description
hasklets draws widgets on the desktop of Wayland compositors with
wlr-layer-shell: weather, CPU/memory/network, media player (MPRIS), disks,
top processes and calendar. Written in Haskell with gi-gtk4 and cairo,
configured with a JSON file and a settings window.


%global main_version %{version}

%if %{defined ghclibdir}
%ghc_lib_subpackage -l Apache-2.0 %{tzdata}
%ghc_lib_subpackage -l Apache-2.0 %{tz}
%ghc_lib_subpackage -l BSD-3-Clause %{gicairorender}
%ghc_lib_subpackage -l LGPL-2.1-only %{gicairoconnector}
%ghc_lib_subpackage -l LGPL-2.1-only %{gipangocairo}
%ghc_lib_subpackage -l LGPL-2.1-only %{gigtk4layershell}
%endif

%global version %{main_version}


%prep
%setup -q -n %{pkgver} -a1 -a2 -a3 -a4 -a5 -a6
cp -bp %{SOURCE12} %{tz}/tz.cabal
cp -bp %{SOURCE14} %{gicairoconnector}/gi-cairo-connector.cabal


%build
# Workaround: Fedora's ghc-gi-gtk4 4.0.12-4 was generated from a GTK 4.21
# development GIR and references four functions that never made it into
# stable GTK (gtk_application_save/forget, gtk_restore_reason_get_type,
# gtk_svg_get_n_states). Nothing here calls them. Drop this once
# ghc-gi-gtk4 is rebuilt against the GTK in the distribution.
#
# gi-gtk4-layer-shell's Setup links the shared gi-gtk4, which is lazily
# bound: let the link through (%%ghc_libs_build reuses a Setup that is
# already there).
(cd %{gigtk4layershell} && ghc --make -package Cabal -dynamic \
   -optl-Wl,--allow-shlib-undefined Setup)
%ghc_libs_build %{subpkgs}
# The hasklets binary links the static gi-gtk4: give it stubs that abort.
cat > gtk-compat.c <<'EOF'
#include <stdio.h>
#include <stdlib.h>
static void missing(const char *f)
{
  fprintf(stderr, "hasklets: %s is not in this GTK\n", f);
  abort();
}
__attribute__((weak)) void gtk_application_save(void *a) { missing("gtk_application_save"); }
__attribute__((weak)) void gtk_application_forget(void *a) { missing("gtk_application_forget"); }
__attribute__((weak)) unsigned long gtk_restore_reason_get_type(void) { missing("gtk_restore_reason_get_type"); return 0; }
__attribute__((weak)) unsigned gtk_svg_get_n_states(void *s) { missing("gtk_svg_get_n_states"); return 0; }
EOF
gcc %{build_cflags} -fPIC -c gtk-compat.c -o gtk-compat.o
export LDFLAGS="${LDFLAGS:-%{build_ldflags}} $PWD/gtk-compat.o"
%ghc_bin_build


%install
%ghc_libs_install %{subpkgs}
%ghc_bin_install


%files
%license LICENSE
%doc README.md
%{_bindir}/%{name}
# Data files of the bundled tzdata library.
%{_datadir}/%{tzdata}/


%changelog
* Mon Nov 23 2026 Carlos Prieto Ortiz <prietus@live.com> - 0.1.2.0-1
- First Fedora package
