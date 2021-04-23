# Copyright: (c) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-only

import grpc
# Generated monocle client with grpc_tools
from monocle import search_pb2 as Monocle
from monocle import search_pb2_grpc as MonocleClient


def run():
    with grpc.insecure_channel('127.0.0.1:8042') as channel:
        stub = MonocleClient.SearchStub(channel)
        response = stub.GetSuggestions(
            Monocle.GetSuggestionsRequest(
                index='openstack'))
    print("Greeter client received: ", response)


if __name__ == '__main__':
    run()
