#!/bin/bash

uptime | sed -e 's!.*load average: !    !g'
