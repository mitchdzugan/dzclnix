#!/usr/bin/env python3

import hashlib
import requests
import json
import os
from pathlib import Path


repo = Path(__file__).resolve().parent.parent


def get_lines(url):
    req = requests.get(url)
    req.raise_for_status()
    for line in req.text.split("\n"):
        line = line.split("#", 1)[0].strip()
        if line == "":
            continue
        yield line


def get_distinfo():
    return {
        k: v
        for k, v in (
            line.split(": ", 1)
            for line in get_lines("http://beta.quicklisp.org/dist/quicklisp.txt")
        )
    }


def get_system_info(distinfo, projects):
    system_info = {}
    for line in get_lines(distinfo["system-index-url"]):
        project, system_file_basename, system_name, *deps = line.split()
        projects[project]["system-deps"][system_name] = deps

        if system_name not in system_info:
            system_info[system_name] = []
        system_info[system_name].append(
            {
                "project": project,
                "system-file-basename": system_file_basename,
            }
        )
    return system_info


def update_cl_cffi_gtk():
    with open(repo / "quicklisp/hashes.json") as f:
        hashes = json.load(f)
    with open(repo / "quicklisp/dist-2023-10-21.json") as f:
        dist_info = json.load(f)
    for project_name, project in dist_info["projects"].items():
        if (project_name != "cl-cffi-gtk"):
            continue
        print(project_name)
        print(project)
        url = "https://github.com/crategus/cl-cffi-gtk/archive/refs/heads/master.tar.gz"
        req = requests.get(url)
        req.raise_for_status()
        md5 = hashlib.md5(req.content).hexdigest()
        print(["md5", md5])
        hashes[md5] = hashlib.sha256(req.content).hexdigest()
        project["url"] = url
        project["md5"] = md5
        project["prefix"] = "cl-cffi-gtk-2024-12-17"
        dist_info["projects"][project_name] = project

    with open(repo / "quicklisp/dist-2023-10-21.json", "w") as f:
        json.dump(dist_info, f)
    with open(repo / "quicklisp/hashes.json", "w") as f:
        json.dump(hashes, f)

update_cl_cffi_gtk()
