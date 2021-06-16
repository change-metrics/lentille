# Monocle.
# Copyright (C) 2021 Monocle authors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# base image
FROM quay.io/change-metrics/builder

# Build project
COPY cabal.project LICENSE /build/
COPY doc/ /build/doc
COPY lentille-bugzilla/ /build/lentille-bugzilla
COPY lentille-github/ /build/lentille-github
COPY lentille-gitlab/ /build/lentille-gitlab

RUN git clone --recurse-submodules https://github.com/change-metrics/monocle /monocle
RUN cd lentille-bugzilla; cabal v2-install -v1 exe:lentille-bugzilla
RUN cd lentille-github;   cabal v2-install -v1 exe:lentille-github
RUN cd lentille-gitlab;   cabal v2-install -v1 exe:lentille-gitlab

################################################################################
FROM registry.fedoraproject.org/fedora:33

COPY --from=0 /root/.cabal/bin/lentille-* /bin/
